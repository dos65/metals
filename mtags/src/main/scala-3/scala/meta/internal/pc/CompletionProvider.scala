package scala.meta.internal.pc

import scala.collection.mutable

import scala.meta.internal.mtags.MtagsEnrichments._
import scala.meta.internal.pc.IdentifierComparator
import scala.meta.pc._

import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.NameOps._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.interactive.Completion
import dotty.tools.dotc.transform.SymUtils._
import dotty.tools.dotc.util.NameTransformer
import dotty.tools.dotc.util.NoSourcePosition
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.Spans
import dotty.tools.dotc.util.SrcPos

class CompletionProvider(
    pos: SourcePosition,
    ctx: Context,
    search: SymbolSearch,
    buildTargetIdentifier: String,
    completionPos: CompletionPos,
    indexedContext: IndexedContext,
    path: List[Tree]
) {

  implicit val context: Context = ctx

  def completions(): (List[CompletionValue], SymbolSearch.Result) = {
    val (_, compilerCompletions) = Completion.completions(pos)

    val (completions, result) =
      compilerCompletions.map(CompletionValue.Compiler(_)).filterInteresting()
    val args = Completions.namedArgCompletions(pos, path, indexedContext)

    val values =
      (completions ++ args).sorted(completionOrdering)
    (values, result)
  }

  private def description(sym: Symbol): String = {
    if (sym.isType) sym.showFullName
    else sym.info.widenTermRefExpr.show
  }

  private def enrichWithSymbolSearch(
      visit: CompletionValue => Boolean
  ): Option[SymbolSearch.Result] = {
    val query = completionPos.query
    completionPos.kind match {
      case CompletionKind.Empty =>
        val filtered = indexedContext.scopeSymbols.filter { sym =>
          val ignoredMethod = sym.isConstructor
          !ignoredMethod && !sym.is(Synthetic)
        }
        filtered.map { sym =>
          val completion =
            Completion(sym.decodedName, description(sym), List(sym))
          visit(CompletionValue.Scope(completion))
        }
        Some(SymbolSearch.Result.INCOMPLETE)
      case CompletionKind.Scope =>
        val visitor = new CompilerSearchVisitor(
          query,
          sym => {
            val completion =
              Completion(sym.decodedName, description(sym), List(sym))
            visit(CompletionValue.Workspace(completion))
          }
        )
        Some(search.search(query, buildTargetIdentifier, visitor))
      case _ => None
    }
  }

  extension (c: Completion) {
    // completionItem method either way takes only head symbol, so why can safely ignore the rest
    def sym: Symbol = c.symbols.head
  }

  extension (s: SrcPos) {
    def isAfter(s1: SrcPos) =
      s.sourcePos.exists && s1.sourcePos.exists && s.sourcePos.point > s1.sourcePos.point
  }

  extension (sym: Symbol) {
    def detailString: String = {
      if (sym.is(Method)) {
        val sig = sym.signature
        val sigString =
          if (sig.paramsSig.isEmpty) "()"
          else
            sig.paramsSig
              .map(p => p.toString)
              .mkString("(", ",", ")")
        sym.showFullName + sigString
      } else {
        sym.fullName.stripModuleClassSuffix.show
      }
    }
  }
  extension (l: List[CompletionValue]) {
    def filterInteresting(): (List[CompletionValue], SymbolSearch.Result) = {
      val isSeen = mutable.Set.empty[String]
      val buf = List.newBuilder[CompletionValue]
      def visit(head: CompletionValue): Boolean = {
        val sym = head.value.sym
        val id = head match {
          case _: CompletionValue.NamedArg =>
            sym.detailString + "="
          case _ =>
            SemanticdbSymbols.symbolName(sym)
        }
        def isNotLocalForwardReference: Boolean =
          !sym.isLocalToBlock ||
            !sym.srcPos.isAfter(pos) ||
            sym.is(Param)

        if (
          !isSeen(id) &&
          !isUninterestingSymbol(sym) &&
          isNotLocalForwardReference
        ) {
          isSeen += id
          buf += head
          true
        } else {
          false
        }
      }

      l.foreach(visit)
      val searchResult =
        enrichWithSymbolSearch(visit).getOrElse(SymbolSearch.Result.COMPLETE)
      (buf.result, searchResult)
    }
  }

  private lazy val isUninterestingSymbol: Set[Symbol] = Set[Symbol](
    defn.Any_==,
    defn.Any_!=,
    defn.Any_##,
    defn.Object_eq,
    defn.Object_ne,
    defn.RepeatedParamClass,
    defn.ByNameParamClass2x,
    defn.Object_notify,
    defn.Object_notifyAll,
    defn.Object_notify,
    defn.Predef_undefined,
    defn.ObjectClass.info.member(nme.wait_).symbol,
    // NOTE(olafur) IntelliJ does not complete the root package and without this filter
    // then `_root_` would appear as a completion result in the code `foobar(_<COMPLETE>)`
    defn.RootPackage,
    // NOTE(gabro) valueOf was added as a Predef member in 2.13. We filter it out since is a niche
    // use case and it would appear upon typing 'val'
    defn.ValueOfClass.info.member(nme.valueOf).symbol
  ).flatMap(_.alternatives.map(_.symbol)).toSet

  private def computeRelevancePenalty(
      completion: CompletionValue
  ): Int = {
    import MemberOrdering._
    val sym = completion.value.sym
    def symbolRelevance(isNamedArg: Boolean): Int = {
      var relevance = 0

      def hasGetter = try {
        def isModuleOrClass = sym.is(Module) || sym.isClass
        // isField returns true for some classes
        def isJavaClass = sym.is(JavaDefined) && isModuleOrClass
        (sym.isField && !isJavaClass && !isModuleOrClass) || sym.getter != NoSymbol
      } catch {
        case _ => false
      }

      // symbols defined in this file are more relevant
      if (!isNamedArg && (pos.source != sym.source || sym.is(Package)))
        relevance |= IsNotDefinedInFile
      // fields are more relevant than non fields
      if (!hasGetter) relevance |= IsNotGetter
      // symbols whose owner is a base class are less relevant
      if (sym.owner == defn.AnyClass || sym.owner == defn.ObjectClass)
        relevance |= IsInheritedBaseMethod
      // symbols not provided via an implicit are more relevant
      if (sym.is(Implicit)) relevance |= IsImplicitConversion
      if (sym.is(Package)) relevance |= IsPackage
      // accessors of case class members are more relevant
      if (!sym.is(CaseAccessor)) relevance |= IsNotCaseAccessor
      // public symbols are more relevant
      if (!sym.isPublic) relevance |= IsNotCaseAccessor
      // synthetic symbols are less relevant (e.g. `copy` on case classes)
      if (sym.is(Synthetic)) relevance |= IsSynthetic
      if (sym.isDeprecated) relevance |= IsDeprecated
      if (isEvilMethod(sym.name)) relevance |= IsEvilMethod
      relevance
    }

    completion match {
      case _: CompletionValue.Workspace =>
        MemberOrdering.IsWorkspaceSymbol + sym.name.show.length
      case _: CompletionValue.NamedArg =>
        symbolRelevance(true) // | IsNamedArg
      // case _: CompletionValue.Scope => symbolRelevance
      case _ => symbolRelevance(false)
    }
  }

  private lazy val isEvilMethod: Set[Name] = Set[Name](
    nme.notifyAll_,
    nme.notify_,
    nme.wait_,
    nme.clone_,
    nme.finalize_
  )

  private def completionOrdering: Ordering[CompletionValue] =
    new Ordering[CompletionValue] {
      val queryLower = completionPos.query.toLowerCase()
      val fuzzyCache = mutable.Map.empty[Symbol, Int]
      def compareLocalSymbols(o1: CompletionValue, o2: CompletionValue): Int = {
        val s1 = o1.value.sym
        val s2 = o2.value.sym
        if (
          s1.isLocal && s2.isLocal && !o1
            .isInstanceOf[CompletionValue.NamedArg] && !o2
            .isInstanceOf[CompletionValue.NamedArg]
        ) {
          if (s1.srcPos.isAfter(s2.srcPos)) -1
          else 1
        } else {
          0
        }
      }
      def fuzzyScore(o: Symbol): Int = {
        fuzzyCache.getOrElseUpdate(
          o, {
            val name = o.name.toString().toLowerCase()
            if (name.startsWith(queryLower)) 0
            else if (name.toLowerCase().contains(queryLower)) 1
            else 2
          }
        )
      }

      //val target = Set(":+", "::")
      val target = Set.empty
      override def compare(o1: CompletionValue, o2: CompletionValue): Int = {
        //val byCompletion = o1.priority - o2.priority
        val debug =
          false //target.contains(o1.value.sym.name.stripModuleClassSuffix.show) && target.contains(o2.value.sym.name.stripModuleClassSuffix.show)
        if (debug)
          println(
            s"${o1.value.sym} ${o1.value.sym.showFullName} --- ${o2.value.sym} ${o2.value.sym.showFullName}"
          )
        val byCompletion = 0
        if (byCompletion != 0) byCompletion
        else {
          val s1 = o1.value.sym
          val s2 = o2.value.sym
          val byLocalSymbol = compareLocalSymbols(o1, o2)
          if (byLocalSymbol != 0) {
            if (debug) println("BY LOCAL")
            byLocalSymbol
          } else {
            val rel1 = computeRelevancePenalty(o1)
            val rel2 = computeRelevancePenalty(o2)
            val byRelevance = Integer.compare(
              rel1,
              rel2
            )
            if (debug)
              println(
                s"byRelevance: ${o1} ${s1.owner}=${rel1} ${MemberOrdering
                  .showFlags(rel1)} / ${o2}${s2.owner} ${rel2} ${MemberOrdering.showFlags(rel2)}"
              )
            if (byRelevance != 0) byRelevance
            else {
              val byFuzzy = Integer.compare(
                fuzzyScore(s1),
                fuzzyScore(s2)
              )
              if (byFuzzy != 0) {
                if (debug) println("byFuzzy")
                byFuzzy
              } else {
                val byIdentifier = IdentifierComparator.compare(
                  s1.name.show,
                  s2.name.show
                )
                if (byIdentifier != 0) {
                  if (debug) println(s"byIdent ${s1} ${s2} $byIdentifier")
                  byIdentifier
                } else {
                  val byOwner =
                    s1.owner.fullName.toString
                      .compareTo(s2.owner.fullName.toString)
                  if (byOwner != 0) byOwner
                  else {
                    val byParamCount = Integer.compare(
                      s1.typeParams.size,
                      s2.typeParams.size
                    )
                    if (byParamCount != 0) byParamCount
                    else {
                      s1.detailString.compareTo(s2.detailString)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
}
