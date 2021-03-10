package scala.meta.internal.mtags

import scala.meta._
import scala.meta.dialects.Scala213
import scala.meta.dialects.Scala3
import scala.meta.inputs.Input
import scala.meta.internal.semanticdb.Language
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.SymbolInformation.Kind
import scala.meta.internal.semanticdb.SymbolInformation.Property
import scala.meta.transversers.SimpleTraverser
import MtagsEnrichments._
import java.net.URI
import java.nio.file.Paths
import scala.util.Try

object ScalaMtags {
  def index(input: Input.VirtualFile): MtagsIndexer = {
    new ScalaMtags(input)
  }
}
class ScalaMtags(val input: Input.VirtualFile)
    extends SimpleTraverser
    with MtagsIndexer {

  // This needs to be further improved with tests
  // https://github.com/scalameta/metals/issues/2493
  private val root: Parsed[Source] = {
    Scala213(input).parse[Source] match {
      case r @ Parsed.Success(_) => r
      case _ => Scala3(input).parse[Source]
    }
  }

  def source: Source = root.get
  override def language: Language = Language.SCALA

  // Scala3 - in case if any toplevel `def`/`val`/`given`/`extension` found
  // remember the owner, name and firstLine for constructing source-toplevel-package symbol
  private var sourceToplevelSym: Option[(String, String, Int)] = None

  override def indexRoot(): Unit = {
    root match {
      case Parsed.Success(tree) =>
        apply(tree)
        sourceToplevelSym.foreach { case (owner, name, startLine) =>
          val pos = Position.Range(input, startLine, 0, startLine, 0)
          withOwner(owner)(term(name, pos, Kind.METHOD, 0))
        }
      case _ => // do nothing in case of parse error
    }
  }
  def currentTree: Tree = myCurrentTree
  private var myCurrentTree: Tree = Source(Nil)

  override def apply(tree: Tree): Unit =
    withOwner() {
      def registerSourceToplevel(): Option[String] = {
        sourceToplevelSym match {
          case None if owner.endsWith("/") =>
            val name = toplevelDefnOwner(currentOwner)
            sourceToplevelSym = Some((currentOwner, name, tree.pos.startLine))
            Some(s"$currentOwner/$name.")
          case _ => None
        }
      }
      def continue(): Unit = super.apply(tree)
      def stop(): Unit = ()
      def enterTermParameters(
          paramss: List[List[Term.Param]],
          isPrimaryCtor: Boolean
      ): Unit = {
        for {
          params <- paramss
          param <- params
        } {
          withOwner() {
            if (isPrimaryCtor) {
              param.name match {
                case name: Term.Name =>
                  term(name, Kind.METHOD, Property.VAL.value)
                case _ =>
              }
            } else {
              super.param(param.name, Kind.PARAMETER, 0)
            }
          }
        }

      }
      def enterTypeParameters(tparams: List[Type.Param]): Unit = {
        for {
          tparam <- tparams
        } {
          withOwner() {
            super.tparam(tparam.name, Kind.TYPE_PARAMETER, 0)
          }
        }
      }
      def enterPatterns(ps: List[Pat], kind: Kind, properties: Int): Unit = {
        ps.foreach { pat =>
          pat.traverse {
            case Pat.Var(name) =>
              withOwner() {
                if (kind.isMethod && properties == Property.VAL.value) {
                  term(name, kind, properties)
                } else {
                  method(name, "()", kind, properties)
                }
              }
            case _ =>
          }
        }
      }
      myCurrentTree = tree
      tree match {
        case _: Source => continue()
        case t: Template =>
          val overloads = new OverloadDisambiguator()
          overloads.disambiguator("") // primary constructor
          def disambiguatedMethod(
              member: Member,
              name: Name,
              tparams: List[Type.Param],
              paramss: List[List[Term.Param]],
              kind: Kind
          ): Unit = {
            val old = myCurrentTree
            myCurrentTree = member
            val disambiguator = overloads.disambiguator(name.value)
            withOwner() {
              method(name, disambiguator, kind, 0)
              enterTypeParameters(tparams)
              enterTermParameters(paramss, isPrimaryCtor = false)
            }
            myCurrentTree = old
          }
          t.stats.foreach {
            case t: Ctor.Secondary =>
              disambiguatedMethod(t, t.name, Nil, t.paramss, Kind.CONSTRUCTOR)
            case t: Defn.Def =>
              disambiguatedMethod(t, t.name, t.tparams, t.paramss, Kind.METHOD)
            case t: Decl.Def =>
              disambiguatedMethod(t, t.name, t.tparams, t.paramss, Kind.METHOD)
            case _ =>
          }
          continue()
        case t: Pkg => pkg(t.ref); continue()
        case t: Pkg.Object =>
          if (currentOwner eq Symbols.EmptyPackage) {
            currentOwner = Symbols.RootPackage
          }
          currentOwner =
            Symbols.Global(currentOwner, Descriptor.Package(t.name.value))
          term("package", t.name.pos, Kind.OBJECT, 0)
          continue()
        case t: Defn.Class =>
          val isImplicit = t.mods.has[Mod.Implicit]
          if (isImplicit) {
            // emit symbol for implicit conversion
            withOwner() {
              method(t.name, "()", Kind.METHOD, Property.IMPLICIT.value)
            }
          }
          tpe(t.name, Kind.CLASS, 0)
          enterTypeParameters(t.tparams)
          enterTermParameters(t.ctor.paramss, isPrimaryCtor = true)
          continue()
        case t: Defn.Trait =>
          tpe(t.name, Kind.TRAIT, 0); continue()
          enterTypeParameters(t.tparams)
        case t: Defn.Object =>
          term(t.name, Kind.OBJECT, 0); continue()
        case t: Defn.Type =>
          val typeOwner = registerSourceToplevel().getOrElse(currentOwner)
          withOwner(typeOwner) {
            tpe(t.name, Kind.TYPE, 0); stop()
            enterTypeParameters(t.tparams)
          }
        case t: Decl.Type =>
          tpe(t.name, Kind.TYPE, 0); stop()
          enterTypeParameters(t.tparams)
        case t: Defn.Val =>
          enterPatterns(t.pats, Kind.METHOD, Property.VAL.value); stop()
        case t: Decl.Val =>
          enterPatterns(t.pats, Kind.METHOD, Property.VAL.value); stop()
        case t: Defn.Var =>
          enterPatterns(t.pats, Kind.METHOD, Property.VAR.value); stop()
        case t: Decl.Var =>
          enterPatterns(t.pats, Kind.METHOD, Property.VAR.value); stop()
        case t: Defn.Enum =>
          tpe(t.name, Kind.CLASS, 0)
        case _: Defn.GivenAlias | _: Defn.Given | _: Defn.ExtensionGroup |
            _: Defn.Def | _: Defn.Val | _: Defn.Var | _: Defn.Type =>
          registerSourceToplevel()
        case _ => stop()
      }
    }

  /**
   * The owner of toplevel definition such as def/val is `$filename$package$`
   */
  private def toplevelDefnOwner(prev: String): String = {
    Try {
      val uri = URI.create(input.path)
      val filename = Paths.get(uri).filename
      val name = filename.stripSuffix(".scala")
      s"$name$$package"
    }.getOrElse("")
  }

}
