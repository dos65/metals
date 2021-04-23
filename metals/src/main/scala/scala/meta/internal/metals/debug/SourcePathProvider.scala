package scala.meta.internal.metals.debug

import scala.meta.internal.metals.BuildTargets
import scala.meta.internal.metals.DefinitionProvider
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.io.AbsolutePath

import ch.epfl.scala.bsp4j.BuildTargetIdentifier
import org.eclipse.lsp4j.debug.Source

final class SourcePathProvider(
    definitionProvider: DefinitionProvider,
    buildTargets: BuildTargets,
    targets: List[BuildTargetIdentifier]
) {
  def findPathFor(source: Source): Option[AbsolutePath] = {
    if (source == null) None
    else {
      searchAsClassPathSymbol(source).orElse(searchAsSourceFile(source))
    }
  }

  private def searchAsClassPathSymbol(source: Source): Option[AbsolutePath] = {
    val base = source.getPath
      .stripSuffix(".scala")
      .stripSuffix(".java")
      .replace("\\", "/") // adapt windows paths to the expected format

    val symbolBase = if (base.contains("/")) base else "_empty_/" + base
    val symbols = for {
      symbol <- Set(symbolBase + ".", symbolBase + "#").toIterator
      location <- definitionProvider.fromSymbol(symbol, targets).asScala
    } yield location.getUri.toAbsolutePath

    if (symbols.isEmpty) {
      scribe.debug(s"no definition for symbol: $symbolBase")
    }

    symbols.headOption
  }

  private def searchAsSourceFile(source: Source): Option[AbsolutePath] = {
    val files = for {
      target <- targets.view
      sourceFile <- buildTargets.buildTargetTransitiveSources(target)
      if sourceFile.filename == source.getName
    } yield sourceFile

    if (files.isEmpty) {
      scribe.debug(s"no matching source file: $source")
    }

    files.headOption
  }
}
