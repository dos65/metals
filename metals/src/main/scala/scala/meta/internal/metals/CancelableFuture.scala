package scala.meta.internal.metals

import scala.concurrent.{ExecutionContext, Future}

case class CancelableFuture[T](
    future: Future[T],
    cancelable: Cancelable = Cancelable.empty
) extends Cancelable {
  def cancel(): Unit = {
    cancelable.cancel()
  }

  def map[B](f: T => B)(implicit ec: ExecutionContext): CancelableFuture[B] = {
    CancelableFuture(future.map(f), cancelable)
  }
}

object CancelableFuture {
  def apply[T](
      thunk: => T
  )(implicit ec: ExecutionContext): CancelableFuture[T] = {
    CancelableFuture(Future(thunk), Cancelable.empty)
  }
  def successful[T](value: T): CancelableFuture[T] =
    CancelableFuture(Future.successful(value))

  def sequence[T](
      fs: Seq[CancelableFuture[T]]
  )(implicit ec: ExecutionContext): CancelableFuture[Seq[T]] = {
    val (f, c) = fs.foldLeft((List.empty[Future[T]], List.empty[Cancelable]))({
      case ((facc, cacc), x) => (x.future :: facc, x.cancelable :: cacc)
    })
    CancelableFuture(
      Future.sequence(f),
      Cancelable(() => Cancelable.cancelAll(c))
    )
  }

}
