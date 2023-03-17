import sbt._
import sbt.Keys._
import sbtbuildinfo.BuildInfoKey
import sbtbuildinfo.BuildInfoKeys.{buildInfoKeys, buildInfoPackage}

object JavaPcSettings {
  lazy val currentJavaHome = settingKey[File]("current java home")
  lazy val currentJavaVersion = settingKey[String]("current java version")

  def settings(sharedSettings: Def.SettingsDefinition): Project => Project = {
    prj: Project =>
      prj.settings(
        sharedSettings,
        moduleName := "mtags-java",
        scalaVersion := V.scala213,
        currentJavaHome := file(System.getProperty("java.home")),
        currentJavaVersion := {
          def fromReleaseFile: Option[String] = {
            import java.{util => ju}
            import scala.io.Source
            import scala.util.control.NonFatal

            val releaseFile = currentJavaHome.value / "release"
            if (releaseFile.exists) {
              val props = new ju.Properties
              props.load(Source.fromFile(releaseFile).bufferedReader())
              try {
                parse(
                  props
                    .getProperty("JAVA_VERSION")
                    .stripPrefix("\"")
                    .stripSuffix("\"")
                )
              } catch {
                case NonFatal(e) =>
                  None
              }
            } else None
          }

          def jdk8Fallback: Option[String] = {
            val rtJar = currentJavaHome.value / "jre" / "lib" / "rt.jar"
            val rt2Jar = currentJavaHome.value / "lib" / "rt.jar"
            if (rtJar.exists || rt2Jar.exists) {
              Some("8")
            } else None
          }

          def parse(v: String): Option[String] = {
            val numbers = v
              .split('-')
              .head
              .split('.')
              .toList
              .take(2)

            numbers match {
              case "1" :: minor :: _ =>
                Some(minor)
              case single :: _ =>
                Some(single)
              case _ => None
            }
          }

          fromReleaseFile.orElse(jdk8Fallback).get
        },
        Compile / unmanagedJars ++= {
          if (currentJavaVersion.value == "8")
            Seq(
              file(
                currentJavaHome.value.getPath
                  .stripSuffix("jre") + "lib/tools.jar"
              )
            )
          else Nil
        },
      )
  }
}
