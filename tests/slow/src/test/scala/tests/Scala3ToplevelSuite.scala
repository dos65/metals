package tests

import scala.meta.internal.io.FileIO
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.Embedded
import scala.meta.internal.metals.{BuildInfo => V}
import scala.meta.internal.metals.UserConfiguration
import scala.meta.internal.mtags.Mtags
import scala.meta.internal.mtags.ScalaMtags
import scala.meta.inputs.Input
import java.nio.charset.StandardCharsets
import scala.collection.mutable
import scala.meta.io.AbsolutePath

class Scala3ToplevelSuite extends munit.FunSuite {

  test("tasty/mtags") {
    val props = InputProperties.scala3()
    val fromTasty = readTasty(props.classDirectory.get)
    fromTasty.foreach { case (src, toplevels) =>
      val srcPath = props.sourceroot.resolve(src)
      val text =
        FileIO.slurp(srcPath, StandardCharsets.UTF_8)
      val input = Input.VirtualFile(srcPath.toURI(false).toString, text)
      val doc = ScalaMtags.index(input).index()
      val fromMtags = Mtags.toplevels(doc)
      assertToplevels(src, toplevels, fromMtags)
    }
  }

  test("tasty/semanticdb") {
    val props = InputProperties.scala3()
    val semanticdbDir = props.semanticdbRoot.get
      .resolve("META-INF")
      .resolve("semanticdb")

    val fromTasty = readTasty(props.classDirectory.get)
    fromTasty.foreach { case (src, toplevels) =>
      val semanticdbPath = semanticdbDir.resolve(s"$src.semanticdb")
      val doc = FileIO.readAllDocuments(semanticdbPath)
      val fromSemanticdb = Mtags.toplevels(doc.seq.head)
      assertToplevels(src, toplevels, fromSemanticdb)
    }
  }

  private def assertToplevels(
      src: String,
      obtained: List[String],
      expected: List[String]
  ): Unit = {
    assertNoDiff(
      obtained.sorted.mkString("\n"),
      expected.sorted.mkString("\n"),
      s"Difference in file: $src"
    )
  }

  private def readTasty(classesDir: AbsolutePath): Map[String, List[String]] = {
    val tastyFiles = FileIO
      .listAllFilesRecursively(classesDir)
      .filter(_.filename.endsWith(".tasty"))
    val tastyInspect = {
      val embedded =
        new Embedded(Embedded.TrackSlowTask.noop, () => UserConfiguration())
      embedded.tastyInspect(V.scala3)
    }

    val inspected = tastyFiles.map(f => tastyInspect.inspectTasty(f.toURI))
    inspected
      .foldLeft(Map.empty[String, mutable.Buffer[String]]) {
        case (acc, result) =>
          val src = result.sourceFilePath
          val existing = acc.getOrElse(src, mutable.Buffer.empty)
          acc.updated(src, result.toplevels().asScala ++ existing)
      }
      .map { case (src, buf) => (src, buf.toList) }
  }

}
