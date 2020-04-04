package scala.meta.internal.metals

import scala.meta.io.AbsolutePath
import scala.concurrent.Future
import scala.meta.internal.builds.BuildTool
import scala.meta.internal.process.ProcessHandler
import scala.collection.JavaConverters._
import com.zaxxer.nuprocess.NuProcessBuilder
import scala.concurrent.ExecutionContext
import scala.meta.internal.process.ExitCodes
import ch.epfl.scala.{bsp4j => b}
import scala.meta.internal.builds.SbtBuildTool
import ch.epfl.scala.bsp4j.DependencySourcesResult
import ch.epfl.scala.bsp4j.BuildTargetIdentifier
import java.io.File
import com.google.gson.Gson

sealed trait BuildImporter {

  def importBuild(
      workspace: AbsolutePath,
      buildTool: BuildTool,
      languageClient: MetalsLanguageClient,
      buildClient: MetalsBuildClient
  ): Future[List[(BuildServerConnection, ImportedBuild)]]

}

object BuildImporter {

  def create(
      userConfig: () => UserConfiguration,
      bspServers: BspServers,
      bloopServes: BloopServers,
      ec: ExecutionContext
  ): BuildImporter = {

    new BuildImporter {

      implicit val currEc = ec

      def importBuild(
          workspace: AbsolutePath,
          buildTool: BuildTool,
          languageClient: MetalsLanguageClient,
          buildClient: MetalsBuildClient
      ): Future[List[(BuildServerConnection, ImportedBuild)]] = {

        for {
          mainInstall <- installBloop(workspace, buildTool, languageClient)
            .flatMap(_ => connect(workspace))
          _ <- sbtSources()
          metaInstall <- buildTool match {
            case sbt: SbtBuildTool =>
              connect(workspace.resolve("project")).map(Some(_))
            case _ => Future.successful(None)
          }
          _ = metaInstall.foreach({
            case (_, b) => {
              b.dependencySources
                .getItems()
                .asScala
                .foreach(ds => {
                  scribe.info(ds.toString())
                })
            }
          })
        } yield List(Some(mainInstall), metaInstall).flatten
      }

      private def connect(
          workspace: AbsolutePath
      ): Future[(BuildServerConnection, ImportedBuild)] = {
        bloopServes
          .newServer(userConfig(), workspace)
          .flatMap({
            case Some(connection) =>
              for {
                workspaceBuildTargets <- connection.workspaceBuildTargets()
                ids = workspaceBuildTargets.getTargets.asScala.map(_.getId)
                scalacOptions <- connection
                  .buildTargetScalacOptions(
                    new b.ScalacOptionsParams(ids.asJava)
                  )
                sources <- connection
                  .buildTargetSources(new b.SourcesParams(ids.asJava))
                dependencySources <- connection
                  .buildTargetDependencySources(
                    new b.DependencySourcesParams(ids.asJava)
                  )
              } yield {
                connection -> ImportedBuild(
                  workspaceBuildTargets,
                  scalacOptions,
                  sources,
                  dependencySources,
                  connection.version,
                  connection.name
                )
              }
            case None =>
              Future
                .failed(new Exception(s"Can connect to bloop ws:$workspace"))
          })
      }

      private def installBloop(
          workspace: AbsolutePath,
          buildTool: BuildTool,
          languageClient: MetalsLanguageClient
      ): Future[BloopInstallResult] = {

        val envs = Map(
          "COURSIER_PROGRESS" -> "disable",
          "METALS_ENABLED" -> "true",
          "SCALAMETA_VERSION" -> BuildInfo.semanticdbVersion
        ) ++ userConfig().javaHome
          .map(v => Map("JAVA_HOME" -> v))
          .getOrElse(Map.empty)

        buildTool.bloopInstall(
          workspace,
          languageClient,
          args => {
            scribe.info(s"running '${args.mkString(" ")}' at $workspace")
            runPs(args, workspace, envs, buildTool.redirectErrorOutput)
              .map({
                case ExitCodes.Success => BloopInstallResult.Installed
                case ExitCodes.Cancel => BloopInstallResult.Cancelled
                case x =>
                  scribe.error(
                    s"$buildTool command failed: ${args.mkString(" ")}"
                  )
                  BloopInstallResult.Failed(x)
              })
          }
        )

      }

      private def sbtSources(): Future[Seq[File]] = {

        import coursierapi._

        Future {
          val dep = Dependency.of("org.scala-sbt", "sbt", "1.3.8")
          Fetch
            .create()
            .withDependencies(dep)
            .withClassifiers(Set("sources").asJava)
            .fetch()
            .asScala
        }
      }

    }

  }

  private def runPs(
      args: List[String],
      pwd: AbsolutePath,
      env: Map[String, String],
      joinErrorsOut: Boolean
  ): Future[Int] = {
    val elapsed = new Timer(Time.system)
    val handler = ProcessHandler(joinErrorWithInfo = joinErrorsOut)
    val pb = new NuProcessBuilder(handler, args.asJava)
    pb.setCwd(pwd.toNIO)
    env.foreach(e => pb.environment.put(e._1, e._2))
    val runningProcess = pb.start()
    handler.completeProcess.future
  }

}
