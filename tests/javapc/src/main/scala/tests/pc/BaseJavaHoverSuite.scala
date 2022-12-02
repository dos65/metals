package tests.pc

import java.nio.file.Paths

import scala.meta.internal.metals.CompilerOffsetParams
import scala.meta.internal.metals.CompilerRangeParams
import scala.meta.internal.mtags.MtagsEnrichments._

import munit.Location
import munit.TestOptions
import tests.TestHovers

class BaseJavaHoverSuite extends BaseJavaPCSuite with TestHovers { // todo: General Logic

  def check(
      testOpt: TestOptions,
      original: String,
      expected: String,
      includeRange: Boolean = false,
      automaticPackage: Boolean = true,
  )(implicit loc: Location): Unit = {
    test(testOpt) {
      val filename = "Hover.java"
      val pkg = packageName(testOpt.name)
      val noRange = original
        .replace("<<", "")
        .replace(">>", "")
      val packagePrefix =
        if (automaticPackage) s"package $pkg;\n"
        else ""
      val codeOriginal = packagePrefix + noRange
      val (code, so, eo) = hoverParams(codeOriginal, filename)

      val pcParams = if (so == eo) {
        CompilerOffsetParams(Paths.get(filename).toUri, code, so)
      } else {
        CompilerRangeParams(Paths.get(filename).toUri, code, so, eo)
      }
      val hover = presentationCompiler
        .hover(pcParams)
        .get()

      val obtained: String =
        renderAsString(code, hover.asScala.map(_.toLsp), includeRange)

      assertNoDiff(
        obtained,
        expected,
      )
    }
  }

  private def packageName(name: String): String = {
    name.toLowerCase.split(" ").mkString("_")
  }
}
