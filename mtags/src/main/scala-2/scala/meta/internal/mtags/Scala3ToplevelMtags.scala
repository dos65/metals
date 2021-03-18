package scala.meta.internal.mtags

import java.nio.file.Paths

import scala.annotation.tailrec

import scala.meta.Dialect
import scala.meta.inputs.Input
import scala.meta.inputs.Position
import scala.meta.internal.inputs._
import scala.meta.internal.mtags.MtagsEnrichments._
import scala.meta.internal.semanticdb.Language
import scala.meta.internal.semanticdb.Scala
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.SymbolInformation.Kind
import scala.meta.internal.tokenizers.LegacyScanner
import scala.meta.internal.tokenizers.LegacyToken._
import scala.meta.tokenizers.TokenizeException

class Scala3ToplevelMtags(
    val input: Input.VirtualFile,
    includeInnerClasses: Boolean,
    dialect: Dialect
) extends MtagsIndexer {

  import Scala3ToplevelMtags._

  override def language: Language = Language.SCALA

  override def indexRoot(): Unit = loop(0, true, Region.RootRegion, None)

  private val scanner = new LegacyScanner(input, dialect)
  scanner.reader.nextChar()
  def isDone: Boolean = scanner.curr.token == EOF

  private var sourceTopLevelAdded = false

  private def resetRegion(region: Region): Region = {
    currentOwner = region.owner
    region
  }

  private def exitIndented(region: Region, indent: Int): Region =
    region match {
      case indented: Region.Indented if indent <= indented.exitIndent =>
        exitIndented(indented.prev, indent)
      case _ => region
    }

  @tailrec
  private def loop(
      indentLevel: Int,
      isAfterNL: Boolean,
      region: Region,
      expectTemplate: Option[ExpectTemplate]
  ): Unit = {
    def newExpectTemplate: ExpectTemplate =
      ExpectTemplate(indentLevel, currentOwner, false)
    def needEmitMember(region: Region): Boolean =
      includeInnerClasses || region.acceptMembers

    if (!isDone) {
      val data = scanner.curr

      val currRegion = data.token match {
        case WHITESPACE => region
        case _ => exitIndented(region, indentLevel)
      }

      data.token match {
        case PACKAGE =>
          val isPackage = emitPackage(currRegion.owner)
          if (isPackage)
            loop(
              indentLevel,
              false,
              Region.Package(currentOwner, currRegion),
              Some(newExpectTemplate.copy(isPackageBody = true))
            )
          else
            loop(indentLevel, false, currRegion, Some(newExpectTemplate))
        case CLASS | TRAIT | OBJECT | ENUM if needEmitMember(currRegion) =>
          emitMember(false, currRegion.owner)
          loop(indentLevel, false, currRegion, Some(newExpectTemplate))
        case DEF | VAL | VAR | GIVEN
            if !sourceTopLevelAdded && currRegion.produceSourceToplevel =>
          sourceTopLevelAdded = true
          val pos = newPosition
          val srcName = Paths.get(input.path).filename.stripSuffix(".scala")
          val name = s"$srcName$$package"
          withOwner() {
            term(name, pos, Kind.OBJECT, 0)
          }
          acceptTrivia()
          scanner.nextToken()
          loop(indentLevel, false, region, expectTemplate)
        case WHITESPACE =>
          if (isNewline) {
            scanner.nextToken()
            loop(0, true, region, expectTemplate)
          } else {
            val nextIndentLevel =
              if (isAfterNL) indentLevel + 1 else indentLevel
            scanner.nextToken()
            loop(nextIndentLevel, isAfterNL, region, expectTemplate)
          }
        case COLON =>
          expectTemplate match {
            case Some(expect) =>
              scanner.nextToken()
              if (isNewline) {
                if (!includeInnerClasses || expect.isPackageBody) {
                  val nextIndent = acceptWhileIndented(expect.indent)
                  loop(nextIndent, false, currRegion, None)
                } else {
                  val prev =
                    if (expect.isPackageBody) currRegion.prev else currRegion
                  val nextRegion = resetRegion(
                    Region.Indented(expect.owner, expect.indent, prev)
                  )
                  scanner.nextToken()
                  loop(0, true, nextRegion, None)
                }
              } else {
                scanner.nextToken()
                loop(indentLevel, false, currRegion, expectTemplate)
              }
            case None =>
              scanner.nextToken()
              loop(indentLevel, false, currRegion, expectTemplate)
          }
        case LBRACKET =>
          acceptBalancedDelimeters(LBRACKET, RBRACKET)
          scanner.nextToken()
          loop(indentLevel, false, currRegion, expectTemplate)
        case LPAREN =>
          acceptBalancedDelimeters(LPAREN, RPAREN)
          scanner.nextToken()
          loop(indentLevel, false, currRegion, expectTemplate)
        case LBRACE =>
          expectTemplate match {
            case Some(expect) if includeInnerClasses || expect.isPackageBody =>
              val prev =
                if (expect.isPackageBody) currRegion.prev else currRegion
              val next = resetRegion(Region.InBrace(expect.owner, prev))
              scanner.nextToken()
              loop(indentLevel, false, next, None)
            case _ =>
              acceptBalancedDelimeters(LBRACE, RBRACE)
              scanner.nextToken()
              loop(indentLevel, false, currRegion, expectTemplate)
          }
        case RBRACE =>
          val nextRegion = currRegion match {
            case Region.InBrace(_, prev) => resetRegion(prev)
            case r => r
          }
          scanner.nextToken()
          loop(indentLevel, isAfterNL, nextRegion, None)
        case _ =>
          val nextExpectTemplate = expectTemplate.filter(!_.isPackageBody)
          scanner.nextToken()
          loop(indentLevel, false, region, nextExpectTemplate)
      }
    } else ()
  }

  def emitPackage(owner: String): Boolean = {
    require(scanner.curr.token == PACKAGE, "package")
    if (currentOwner eq Symbols.EmptyPackage) {
      currentOwner = Symbols.RootPackage
    }
    currentOwner = owner
    acceptTrivia()
    scanner.curr.token match {
      case IDENTIFIER | BACKQUOTED_IDENT =>
        val paths = parsePath()
        paths.foreach { path => pkg(path.name, path.pos) }
        true
      case OBJECT =>
        emitMember(isPackageObject = true, owner)
        false
      case _ =>
        require(isOk = false, "package name or package object")
        false
    }
  }

  /**
   * Consume token stream like "a.b.c" and return List(a, b, c)
   */
  def parsePath(): List[Identifier] = {
    val buf = List.newBuilder[Identifier]
    def loop(): Unit = {
      buf += newIdentifier
      acceptTrivia()
      scanner.curr.token match {
        case DOT =>
          acceptTrivia()
          loop()
        case _ =>
      }
    }
    loop()
    buf.result()
  }

  /**
   * Enters a toplevel symbol such as class, trait or object
   */
  def emitMember(isPackageObject: Boolean, owner: String): Unit = {
    val kind = scanner.curr.token
    acceptTrivia()
    val name = newIdentifier
    currentOwner = owner
    //println(s"EMIT MEMBER: ${name.name}")
    kind match {
      case CLASS =>
        tpe(name.name, name.pos, Kind.CLASS, 0)
      case TRAIT =>
        tpe(name.name, name.pos, Kind.TRAIT, 0)
      case ENUM =>
        tpe(name.name, name.pos, Kind.CLASS, 0)
      case OBJECT =>
        if (isPackageObject) {
          currentOwner = symbol(Scala.Descriptor.Package(name.name))
          term("package", name.pos, Kind.OBJECT, 0)
        } else {
          term(name.name, name.pos, Kind.OBJECT, 0)
        }
    }
    scanner.nextToken()
  }

  /**
   * Consumes the token stream until the matching closing delimiter
   */
  def acceptBalancedDelimeters(Open: Int, Close: Int): Unit = {
    require(scanner.curr.token == Open, "open delimeter { or (")
    var count = 1
    while (!isDone && count > 0) {
      scanner.nextToken()
      scanner.curr.token match {
        case Open =>
          count += 1
        case Close =>
          count -= 1
        case _ =>
      }
    }
  }

  def acceptWhileIndented(exitIndent: Int): Int = {
    @tailrec
    def loop(indent: Int, isAfterNL: Boolean): Int = {
      //println(s"loopind: ${currentToken} $indent $isAfterNL")
      if (!isDone) {
        scanner.curr.token match {
          case WHITESPACE =>
            if (isNewline) { scanner.nextToken; loop(0, true) }
            else if (isAfterNL) { scanner.nextToken; loop(indent + 1, true) }
            else { scanner.nextToken(); loop(indent, false) }

          // val nextIndent = if (isAfterNL) indent + 1 else indent
          // scanner.nextToken()
          // loop(nextIndent, isAfterNL || isNewline)
          case _ if indent <= exitIndent => indent
          case _ =>
            scanner.nextToken()
            loop(indent, false)
        }
      } else indent
    }
    loop(0, true)
  }

  private def acceptTrivia(): Unit = {
    scanner.nextToken()
    while (
      !isDone &&
      (scanner.curr.token match {
        case WHITESPACE | COMMENT => true
        case _ => false
      })
    ) {
      scanner.nextToken()
    }
  }

  /**
   * Returns a name and position for the current identifier token
   */
  def newIdentifier: Identifier = {
    scanner.curr.token match {
      case IDENTIFIER | BACKQUOTED_IDENT => // OK
      case _ => fail("identifier")
    }
    val pos = newPosition
    val name = scanner.curr.name
    new Identifier(name, pos)
  }

  private def isNewline: Boolean =
    scanner.curr.token == WHITESPACE &&
      (scanner.curr.strVal match {
        case "\n" | "\r" => true
        case _ => false
      })

  def fail(expected: String): Nothing = {
    throw new TokenizeException(newPosition, failMessage(expected))
  }

  def failMessage(expected: String): String = {
    newPosition.formatMessage(
      "error",
      s"expected $expected; obtained $currentToken"
    )
  }

  /**
   * Returns position of the current token
   */
  def newPosition: Position = {
    val start = scanner.curr.offset
    val end = scanner.curr.endOffset + 1
    Position.Range(input, start, end)
  }

  def currentToken: String =
    InverseLegacyToken.category(scanner.curr.token).toLowerCase()

  def require(isOk: Boolean, expected: String): Unit = {
    if (!isOk) {
      throw new TokenizeException(newPosition, failMessage(expected))
    }
  }
}

object Scala3ToplevelMtags {

  final case class ExpectTemplate(
      indent: Int,
      owner: String,
      isPackageBody: Boolean
  )

  sealed trait Region {
    def prev: Region
    def owner: String
    def acceptMembers: Boolean
    def produceSourceToplevel: Boolean
  }

  object Region {

    case object RootRegion extends Region { self =>
      val owner: String = Symbols.EmptyPackage
      val prev: Region = self
      val acceptMembers: Boolean = true
      val produceSourceToplevel: Boolean = true
    }

    final case class Package(owner: String, prev: Region) extends Region {
      val acceptMembers: Boolean = true
      val produceSourceToplevel: Boolean = true
    }

    final case class InBrace(owner: String, prev: Region) extends Region {
      def acceptMembers: Boolean =
        owner.endsWith("/") // && owner.endsWith("package.")
      val produceSourceToplevel: Boolean = false
    }
    final case class Indented(owner: String, exitIndent: Int, prev: Region)
        extends Region {
      def acceptMembers: Boolean =
        owner.endsWith("/") // && owner.endsWith("package.")
      val produceSourceToplevel: Boolean = false
    }
  }
}
