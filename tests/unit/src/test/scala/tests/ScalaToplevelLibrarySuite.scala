package tests

import scala.meta.dialects
import scala.meta.internal.io.FileIO
import scala.meta.internal.mtags.Mtags
import scala.meta.internal.mtags.MtagsEnrichments._
import scala.meta.io.AbsolutePath

/**
 * Assert the symbols emitted by ScalaToplevelMtags is a subset of ScalaMtags
 */
class ScalaToplevelLibrarySuite extends BaseSuite {
  val testClasspath: List[AbsolutePath] = Library.all.flatMap(_.sources.entries)
  testClasspath.foreach { entry =>
    test(entry.toNIO.getFileName.toString) {
      FileIO.withJarFileSystem(entry, create = false) { root =>
        FileIO.listAllFilesRecursively(root).foreach { file =>
          if (file.toNIO.getFileName.toString.endsWith(".scala")) {
            val input = file.toInput
            val scalaMtags = Mtags.toplevels(Mtags.index(input))
            val scalaToplevelMtags = Mtags.toplevels(input)
            val scala3ToplevelMtags =
              Mtags.toplevels(input, dialect = dialects.Scala3)
            val obtained = scalaToplevelMtags.mkString("\n")
            val obtained2 = scala3ToplevelMtags.mkString("\n")
            val expected = scalaMtags.mkString("\n")
            assertNoDiff(obtained, expected, input.text)
            assertNoDiff(obtained2, obtained, input.text)
          }
        }
      }
    }
  }

}
