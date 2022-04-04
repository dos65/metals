package scala.meta.internal.signatures

import ch.epfl.scala.{bsp4j => b}
import scala.concurrent.Future
import scala.meta.internal.metals.BuildTargets
import scala.meta.io.AbsolutePath
import scala.meta.internal.metals.Directories
import java.nio.file.Files
import scala.meta.internal.metals.ScalaTarget
import scala.meta.internal.process.SystemProcess
import scala.meta.internal.metals.Embedded
import java.io.File
import scala.meta.internal.io.FileIO
import scala.concurrent.ExecutionContext
import scala.meta.internal.metals.MetalsEnrichments._

class SigCompilations(
    buildTargets: BuildTargets,
    workspace: () => AbsolutePath
)(implicit ec: ExecutionContext) {

  private val jars = Embedded.downloadScalaScanner("2.13.8")

  def sigPathsFor(info: b.BuildTarget): AbsolutePath = {
    val dir =
      workspace()
        .resolve(Directories.signaturesPath)
        .resolve(info.getDisplayName)

    if (!Files.exists(dir.toNIO))
      Files.createDirectories(dir.toNIO)

    dir
  }

  def compile(targets: Seq[b.BuildTargetIdentifier]): Future[Unit] = {
    Future
      .sequence(
        targets.toList.map { target =>
          buildTargets.scalaTarget(target) match {
            case None => Future.unit
            case Some(scalaTarget) => runSigCompiler(scalaTarget)
          }
        }
      )
      .map(_ => ())
  }

  private def runSigCompiler(target: ScalaTarget): Future[Unit] = {
    val dir = sigPathsFor(target.info)
    val sourcePaths = buildTargets.buildTargetSources(target.info.getId())

    val allSources = sourcePaths
      .flatMap(f =>
        if (f.isFile) List(f)
        else FileIO.listAllFilesRecursively(f)
      )
      .filter(_.isScala)

    if (allSources.isEmpty) {
      Future.unit
    } else {
      val cmd = List(
        "java",
        "-cp",
        jars.mkString(File.pathSeparator),
        "scala.meta.scanner.Main",
        "-usejavacp",
        "-Ystop-after:pickler",
        "-Ypickle-write",
        dir.toString
      ) ++ allSources.map(_.toString)

      // scribe.info(
      //   s"SIG COMPILE FOR: ${target.info.getDisplayName}:\n ${cmd.mkString("\n")}"
      // )
      val ps = SystemProcess.run(
        cmd,
        workspace(),
        false,
        Map.empty,
        scribe.info(_),
        scribe.error(_),
        false
      )
      ps.complete.map(_ => ())
    }
  }

}
