package scala.meta.internal.metals

import scala.util.Try

sealed trait BuildTargetMetadata

object BuildTargetMetadata {

  final case class ScalaBuildTargetMetadata(
      scalaOrganization: String,
      scalaVersion: String,
      scalaBinaryVersion: String,
      platform: Int,
      jars: List[String]
  ) extends BuildTargetMetadata

  final case class SbtBuildTargetMetadata(
      sbtVersion: String,
      autoImports: List[String]
  ) extends BuildTargetMetadata

  implicit val scalaBTRW = upickle.default.macroRW[ScalaBuildTargetMetadata]
  implicit val sbtBTRW = upickle.default.macroRW[SbtBuildTargetMetadata]

  def fromJson(
      dataKind: String,
      jsonData: String
  ): Either[Throwable, BuildTargetMetadata] = {
    Try {
      val json = ujson.read(jsonData)
      dataKind match {
        case "scala" => upickle.default.read[ScalaBuildTargetMetadata](json)
        case "sbt" => upickle.default.read[SbtBuildTargetMetadata](json)
        case x => throw new Exception(s"Uknown datakind $dataKind")
      }
    }.toEither
  }
}
