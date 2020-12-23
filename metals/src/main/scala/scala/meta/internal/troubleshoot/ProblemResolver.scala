package scala.meta.internal.troubleshoot

import scala.collection.mutable.ListBuffer

import scala.meta.internal.bsp.BspSession
import scala.meta.internal.metals.BuildInfo
import scala.meta.internal.metals.Messages
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.ScalaTarget
import scala.meta.internal.metals.ScalaVersions
import scala.meta.internal.semver.SemVer
import scala.meta.io.AbsolutePath

class ProblemResolver(
    workspace: AbsolutePath,
    currentBuildServer: () => Option[BspSession],
    isClientCommandSupported: Boolean
) {

  def isUnsupportedBloopVersion(): Boolean = {
    currentBuildServer() match {
      case Some(bspSession) =>
        bspSession.main.name == "Bloop" && !SemVer.isCompatibleVersion(
          BuildInfo.bloopVersion,
          bspSession.main.version
        )
      case None =>
        false
    }
  }

  def recommendation(scala: ScalaTarget): String = {
    findProblem(scala)
      .map(_.message)
      .getOrElse("")
  }

  def problemMessage(allTargets: List[ScalaTarget]): Option[String] = {

    val unsupportedVersions = ListBuffer[String]()
    val deprecatedVersions = ListBuffer[String]()
    val futureVersions = ListBuffer[String]()
    var misconfiguredProjects = 0
    var unsupportedSbt = false
    var deprecatedSbt = false
    var futureSbt = false

    for {
      target <- allTargets
      issue <- findProblem(target)
    } yield {
      issue match {
        case UnsupportedScalaVersion(version) => unsupportedVersions += version
        case DeprecatedScalaVersion(version) => deprecatedVersions += version
        case FutureScalaVersion(version) => futureVersions += version
        case _: SemanticDBDisabled => misconfiguredProjects += 1
        case _: MissingSourceRoot => misconfiguredProjects += 1
        case UnsupportedSbtVersion => unsupportedSbt = true
        case DeprecatedSbtVersion => deprecatedSbt = true
        case FutureSbtVersion => futureSbt = true
      }
    }

    val unsupportedMessage = if (unsupportedVersions.nonEmpty) {
      Some(Messages.UnsupportedScalaVersion.message(unsupportedVersions.toSet))
    } else {
      None
    }
    val deprecatedMessage = if (deprecatedVersions.nonEmpty) {
      Some(Messages.DeprecatedScalaVersion.message(deprecatedVersions.toSet))
    } else {
      None
    }

    val futureMessage = if (futureVersions.nonEmpty) {
      Some(Messages.FutureScalaVersion.message(futureVersions.toSet))
    } else {
      None
    }

    val deprecatedSbtMessage =
      if (deprecatedSbt) Some(Messages.DeprecatedSbtVersion.message) else None
    val unsupportedSbtMessage =
      if (deprecatedSbt) Some(Messages.UnsupportedSbtVersion.message) else None
    val futureSbtMessage =
      if (deprecatedSbt) Some(Messages.FutureSbtVersion.message) else None

    val semanticdbMessage =
      if (
        misconfiguredProjects == allTargets.size && misconfiguredProjects > 0
      ) {
        Some(Messages.CheckDoctor.allProjectsMisconfigured)
      } else if (misconfiguredProjects == 1) {
        val name = allTargets
          .find(t => !t.isSemanticdbEnabled)
          .map(_.displayName)
          .getOrElse("<none>")
        Some(Messages.CheckDoctor.singleMisconfiguredProject(name))
      } else if (misconfiguredProjects > 0) {
        Some(
          Messages.CheckDoctor.multipleMisconfiguredProjects(
            misconfiguredProjects
          )
        )
      } else {
        None
      }

    val allMessages = List(
      deprecatedMessage,
      unsupportedMessage,
      futureMessage,
      deprecatedSbtMessage,
      unsupportedSbtMessage,
      futureSbtMessage,
      semanticdbMessage
    ).flatten

    def scalaVersionsMessages = List(
      deprecatedMessage,
      unsupportedMessage,
      futureMessage
    ).flatten

    allMessages match {
      case single :: Nil => Some(single)
      case Nil => None
      case messages if messages == scalaVersionsMessages =>
        Some(
          s"Your build definition contains multiple unsupported and deprecated Scala versions."
        )
      case _ =>
        Some(
          s"Multiple problems detected in your build."
        )
    }
  }

  private def findProblem(
      scalaTarget: ScalaTarget
  ): Option[ScalaProblem] = {
    scalaTarget.scalaVersion match {
      case version
          if ScalaVersions.isFutureVersion(version) && scalaTarget.isSbt =>
        Some(FutureSbtVersion)
      case version if ScalaVersions.isFutureVersion(version) =>
        Some(FutureScalaVersion(version))
      case version
          if !ScalaVersions.isSupportedScalaVersion(
            version
          ) && scalaTarget.isSbt =>
        Some(UnsupportedSbtVersion)
      case version if !ScalaVersions.isSupportedScalaVersion(version) =>
        Some(UnsupportedScalaVersion(version))
      case version if !scalaTarget.isSemanticdbEnabled =>
        Some(
          SemanticDBDisabled(
            version,
            currentBuildServer().map(_.main.name).getOrElse("<none>"),
            isUnsupportedBloopVersion()
          )
        )
      case _ if !scalaTarget.isSourcerootDeclared =>
        Some(MissingSourceRoot(workspace.sourcerootOption))
      case version
          if ScalaVersions.isDeprecatedScalaVersion(
            version
          ) && scalaTarget.isSbt =>
        Some(DeprecatedSbtVersion)
      case version if ScalaVersions.isDeprecatedScalaVersion(version) =>
        Some(DeprecatedScalaVersion(version))
      case _ => None
    }
  }
}
