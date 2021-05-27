package scala.meta.internal.pc

import java.util.concurrent.CompletableFuture
import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import java.util.logging.Level
import java.util.logging.Logger

import scala.concurrent.ExecutionContextExecutor
import scala.util.control.NonFatal

import scala.meta.pc.CancelToken
import scala.meta.pc.PresentationCompilerConfig
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.StandardOpenOption
import java.util.concurrent.atomic.AtomicReference


class StopException extends Exception
/**
 * Manages the lifecycle and multi-threaded access to the presentation compiler.
 *
 * - automatically restarts the compiler on miscellaneous crashes.
 * - handles cancellation via `Thread.interrupt()` to stop the compiler during typechecking,
 *   for functions that support cancellation.
 */
abstract class CompilerAccess[Reporter, Compiler](
    config: PresentationCompilerConfig,
    sh: Option[ScheduledExecutorService],
    newCompiler: () => CompilerWrapper[Reporter, Compiler]
)(implicit ec: ExecutionContextExecutor) {
  private val logger: Logger =
    Logger.getLogger(classOf[CompilerAccess[_, _]].getName)

  private val jobs = CompilerJobQueue()

  private val shutdownFlag: AtomicBoolean = new AtomicBoolean(false)

  @volatile
  private var _compiler: CompilerWrapper[Reporter, Compiler] = _
  private def isEmpty: Boolean = _compiler == null
  private def isDefined: Boolean = !isEmpty
  private def loadCompiler(): CompilerWrapper[Reporter, Compiler] = {
    if (shutdownFlag.get()) {
      throw new StopException
    } else {
      if (_compiler == null) {
        _compiler = newCompiler()
        log(s"LOAD COMPLIER ${_compiler.compiler().hashCode()}")
      }
      _compiler.resetReporter()
      _compiler
    }
  }

  protected def newReporter: Reporter

  def reporter: Reporter =
    if (isEmpty) newReporter
    else _compiler.reporterAccess.reporter

  def isLoaded(): Boolean = _compiler != null

  def shutdown(): Unit = {
    shutdownFlag.set(true)
    shutdownCurrentCompiler()
    jobs.shutdown()
  }

  def log(s: String): Unit = 
    Files.write(Paths.get("/home/dos65/wtf_metals.log"), s"$s\n".getBytes(), StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND)

  def shutdownCurrentCompiler(): Unit = {
    val compiler = _compiler
    log(s"TRY SHUTDOWN: ${if (compiler != null) compiler.hashCode else "null"}")
    if (compiler != null) {
      compiler.askShutdown()
      log(s"SHUTDOWN CURRECT COMPILER: ${compiler.hashCode()}")
      _compiler = null
      sh.foreach { scheduler =>
        scheduler.schedule[Unit](
          () => {
            val isAlive = compiler.isAlive()
            if (isAlive) {
              log(s"CALL STOP: ${compiler.hashCode()}")
              compiler.stop()
            }
          },
          2,
          TimeUnit.SECONDS
        )
      }
    }
  }

  /**
   * Asynchronously execute a function on the compiler thread with `Thread.interrupt()` cancellation.
   */
  def withInterruptableCompiler[T](
      default: T,
      token: CancelToken
  )(thunk: CompilerWrapper[Reporter, Compiler] => T): CompletableFuture[T] = {
    val isFinished = new AtomicBoolean(false)
    var queueThread = Option.empty[Thread]
    val result = onCompilerJobQueue(
      () => {
        queueThread = Some(Thread.currentThread())
        try withSharedCompiler(default)(thunk)
        finally isFinished.set(true)
      },
      token
    )
    // Interrupt the queue thread
    token.onCancel.whenCompleteAsync(
      (isCancelled, _) => {
        queueThread.foreach { thread =>
          if (
            isCancelled &&
            isFinished.compareAndSet(false, true) &&
            isDefined
          ) {
            _compiler.presentationCompilerThread.foreach(_.interrupt())
            if (
              _compiler.presentationCompilerThread.isEmpty || !_compiler.presentationCompilerThread
                .contains(thread)
            ) {
              thread.interrupt()
            }
          }
        }
      },
      ec
    )
    result
  }

  /**
   * Asynchronously execute a function on the compiler thread without `Thread.interrupt()` cancellation.
   *
   * Note that the function is still cancellable.
   */
  def withNonInterruptableCompiler[T](
      default: T,
      token: CancelToken
  )(thunk: CompilerWrapper[Reporter, Compiler] => T): CompletableFuture[T] = {
    onCompilerJobQueue(() => withSharedCompiler(default)(thunk), token)
  }

  /**
   * Execute a function on the current thread without cancellation support.
   *
   * May potentially run in parallel with other requests, use carefully.
   */
  def withSharedCompiler[T](
      default: T
  )(thunk: CompilerWrapper[Reporter, Compiler] => T): T = {
    try {
      thunk(loadCompiler())
    } catch {
      case _: StopException => default
      case InterruptException() =>
        default
      case other: Throwable =>
        handleSharedCompilerException(other)
          .map { message =>
            retryWithCleanCompiler(
              thunk,
              default,
              message
            )
          }
          .getOrElse {
            handleError(other)
            default
          }
    }
  }

  protected def handleSharedCompilerException(t: Throwable): Option[String]

  protected def ignoreException(t: Throwable): Boolean

  private def retryWithCleanCompiler[T](
      thunk: CompilerWrapper[Reporter, Compiler] => T,
      default: T,
      cause: String
  ): T = {
    shutdownCurrentCompiler()
    logger.log(
      Level.INFO,
      s"compiler crashed due to $cause, retrying with new compiler instance."
    )
    try thunk(loadCompiler())
    catch {
      case InterruptException() =>
        default
      case NonFatal(e) =>
        handleError(e)
        default
    }
  }

  private def handleError(e: Throwable): Unit = {
    CompilerThrowable.trimStackTrace(e)
    logger.log(Level.SEVERE, e.getMessage, e)
    shutdownCurrentCompiler()
  }

  private def onCompilerJobQueue[T](
      thunk: () => T,
      token: CancelToken
  ): CompletableFuture[T] = {
    val result = new CompletableFuture[T]()
    jobs.submit(
      result,
      { () =>
        token.checkCanceled()
        Thread.interrupted() // clear interrupt bit
        result.complete(thunk())
        ()
      }
    )

    // User cancelled task.
    token.onCancel.whenCompleteAsync(
      (isCancelled, _) => {
        if (isCancelled && !result.isDone()) {
          result.cancel(false)
        }
      },
      ec
    )
    // Task has timed out, cancel this request and shutdown the current compiler.
    sh.foreach { scheduler =>
      scheduler.schedule[Unit](
        { () =>
          if (!result.isDone()) {
            try {
              result.cancel(false)
              shutdownCurrentCompiler()
            } catch {
              case NonFatal(_) =>
              case other: Throwable =>
                if (!ignoreException(other)) throw other
            }
          }
        },
        config.timeoutDelay(),
        config.timeoutUnit()
      )
    }

    result
  }
}
