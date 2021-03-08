package scala.meta.internal.metals

import dotty.tools.dotc.core.tasty.DottyUnpickler
import dotty.tools.dotc.core.tasty.TastyPrinter
import dotty.tools.dotc.core.tasty.PositionUnpickler
import dotty.tools.dotc.core.tasty.TastyUnpickler
import dotty.tools.dotc.core.tasty.TastyUnpickler._
import scala.tasty.inspector.TastyInspector
import scala.tasty.inspector.Inspector
import scala.quoted._
import scala.tasty.inspector._
import java.net.URI
import java.nio.file._
import scala.meta.internal.{semanticdb => s}
import dotty.tools.tasty.TastyFormat.{ASTsSection, PositionsSection, CommentsSection}
import dotty.tools.tasty.{TastyBuffer, TastyReader}
import TastyBuffer.{Addr, NameRef}
import scala.collection.mutable
import scala.meta.internal.mtags.OverloadDisambiguator
import scala.meta.scala3.{TastyInspect => JTastyInspect, _}
import scala.jdk.CollectionConverters._

class TastyInspect extends JTastyInspect:

  def inspectTasty(filePath: URI): InspectTastyResult = {
    val bytes = Files.readAllBytes(Paths.get(filePath))
    val source = ToplevelTastyReader.readSourceName(bytes)

    val acc = mutable.ArrayBuffer.empty[String]
    val inspector = new ToplevelInspector(acc)
    TastyInspector.inspectTastyFiles(List(Paths.get(filePath).toString))(inspector)

    new InspectTastyResult {
      val toplevels = acc.asJava 
      val sourceFilePath = source
    }
  }
  
  class ToplevelInspector(acc: mutable.ArrayBuffer[String]) extends Inspector:

    def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
      tastys.foreach(t => {
        extractToplevels2(t.ast, None)
      })

    def extractToplevels2(using Quotes)(tree: quotes.reflect.Tree, owner: Option[String]): Unit = 
      import quotes.reflect._
      
      tree match {
        case t: PackageClause =>
          val name = if (t.pid.symbol.name == "<empty>") "_empty_" else t.pid.symbol.name
          t.stats.foreach { stat => 
            extract(stat, s"$name/", new OverloadDisambiguator)
          }
        case _ =>
      }

      def extract(using Quotes)(
        tree: quotes.reflect.Tree,
        owner: String,
        disambiguator: OverloadDisambiguator
      ): Unit = {
        import quotes.reflect._

        def name: String = tree.symbol.name
        def isSynthetic: Boolean = tree.symbol.flags.is(Flags.Synthetic)

        tree match {
          case t : ClassDef if !isSynthetic =>
            val fixedName = name.stripSuffix("$")
            val discriminator = if (t.symbol.flags.is(Flags.Module)) "." else "#"
            acc += s"$owner$fixedName$discriminator"
          // case t: ValDef =>
          //   if (!t.symbol.flags.is(Flags.Module))
          //     acc += s"$owner$name."
          // case t: DefDef if !isSynthetic =>
          //   val defName = s"$name${disambiguator.disambiguator(name)}"
          //   acc += s"$owner$defName."
          case t: TypeDef =>
            acc += s"$owner$name#"
          case _ =>
        }
      }
    

object ToplevelTastyReader:

  def readSourceName(bytes: Array[Byte]): String = {
    val unpickler: TastyUnpickler = new TastyUnpickler(bytes)
    
    class PositionSectionUnpickler extends SectionUnpickler[String](PositionsSection) {

      def unpickle(reader: TastyReader, tastyName: NameTable): String = {
        val posUnpickler = new PositionUnpickler(reader, tastyName)
        posUnpickler.sourcePathAt(reader.currentAddr)
      }
    }
    unpickler.unpickle(new PositionSectionUnpickler).getOrElse("")
  }
