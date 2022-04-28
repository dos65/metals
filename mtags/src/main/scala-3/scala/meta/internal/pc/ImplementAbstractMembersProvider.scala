package scala.meta.internal.pc

import scala.meta.internal.mtags.MtagsEnrichments.*
import dotty.tools.dotc.interactive.InteractiveDriver
import scala.meta.pc.OffsetParams
import java.nio.file.Paths
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.ast.Trees.Template
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Types.NameFilter
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.TypeProxy
import dotty.tools.dotc.core.Types.ClassInfo
import dotty.tools.dotc.core.Types.takeAllFilter
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.nonClassTypeNameFilter
import dotty.tools.dotc.core.Types.abstractTermNameFilter
import scala.meta.io.AbsolutePath
import org.eclipse.{lsp4j as l}

object ImplementAbstractMembersProvider:

  extension (sym: Symbol)
    private def isImplementedIn(clazz: ClassSymbol)(using Context) =
      val mbrDenot = sym.denot.asSeenFrom(clazz.thisType)
      def isConcrete(sym: Symbol) = sym.exists && !sym.isOneOf(NotConcrete)
      clazz
        .nonPrivateMembersNamed(sym.name)
        .filterWithPredicate(impl =>
          isConcrete(impl.symbol)
            && mbrDenot.matchesLoosely(impl, alwaysCompareTypes = true)
        )
        .exists
  end extension

  def implementAll(
      driver: InteractiveDriver,
      params: OffsetParams
  ): List[l.TextEdit] =

    val uri = params.uri
    println(uri)
    // val filePath = Paths.get(uri)
    driver.run(
      uri,
      CompilerInterfaces.toSource(params.uri, params.text)
    )
    val unit = driver.currentCtx.run.units.head
    val tree = unit.tpdTree
    val newctx = driver.currentCtx.fresh.setCompilationUnit(unit)

    val pos = driver.sourcePosition(params)

    val path =
      Interactive.pathTo(newctx.compilationUnit.tpdTree, pos.span)(using newctx)

    val indexedContext = IndexedContext(
      MetalsInteractive.contextOfPath(path)(using newctx)
    )
    import indexedContext.ctx

    val missingSymbols = path match
      case (td @ tpd.TypeDef(x, t: tpd.Template)) :: _ =>
        val clazz = td.tpe.classSymbol.asClass

        def isImplemented(mbr: Symbol) =
          val mbrDenot = mbr.asSeenFrom(clazz.thisType)
          def isConcrete(sym: Symbol) = sym.exists && !sym.isOneOf(NotConcrete)
          clazz
            .nonPrivateMembersNamed(mbr.name)
            .filterWithPredicate(impl =>
              isConcrete(impl.symbol)
                && mbrDenot.matchesLoosely(impl, alwaysCompareTypes = true)
            )
            .exists

        // TODO: can't have decls of baseClasses if we didn't open the file with the current context
        val members = td.tpe.allMembers.iterator.toList

        val missingSyms = for
          baseClass <- clazz.baseClasses
          sym <-
            baseClass.info.decls.toList // TODO: can't have decls of baseClasses if we didn't open the file with the current context
          if sym.is(DeferredTerm) && !isImplemented(sym)
        yield sym
        println("~~~~~~~~~~~~~~~~")
        println(missingSyms)
        println("~~~~~~~~~~~~~~~~")
        missingSyms
        Nil
      case _ => Nil
    Nil

  end implementAll
end ImplementAbstractMembersProvider
