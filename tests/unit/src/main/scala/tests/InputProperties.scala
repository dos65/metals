package tests

import scala.meta.internal.io.FileIO
import scala.meta.internal.io.PathIO
import scala.meta.io.AbsolutePath
import scala.meta.io.Classpath

import tests.MetalsTestEnrichments._

case class InputProperties(
    sourceroot: AbsolutePath,
    sourceDirectories: List[AbsolutePath],
    classDirectory: Option[AbsolutePath],
    classpath: Classpath,
    dependencySources: Classpath,
    semanticdbRoot: Option[AbsolutePath]
) {

  def scalaFiles: List[InputFile] = {
    allFiles.filter(file => PathIO.extension(file.file.toNIO) == "scala")
  }

  def allFiles: List[InputFile] = {
    for {
      directory <- sourceDirectories
      if directory.isDirectory
      file <- FileIO.listAllFilesRecursively(directory)
    } yield {
      InputFile(
        file = file,
        file.text,
        directory,
        semanticdbRelativePath = file.toRelative(sourceroot)
      )
    }
  }
}

object InputProperties {

  def scala2(): InputProperties =
    fromResource("metals-input.properties")

  def scala3(): InputProperties =
    fromResource("metals-input3.properties")

  def fromResource(path: String): InputProperties = {
    val props = new java.util.Properties()
    val in = this.getClass.getClassLoader.getResourceAsStream(path)
    assert(in != null, s"no such resource: $path")
    try props.load(in)
    finally in.close()

    def getKeyOpt(key: String): Option[String] =
      Option(props.getProperty(key))
    def getKey(key: String): String = {
      getKeyOpt(key).getOrElse {
        throw new IllegalArgumentException(props.toString)
      }
    }
    InputProperties(
      sourceroot = AbsolutePath(getKey("sourceroot")),
      sourceDirectories = Classpath(getKey("sourceDirectories")).entries,
      classDirectory = getKeyOpt("classDirectory").map(AbsolutePath(_)),
      classpath = Classpath(getKey("classpath")),
      dependencySources = Classpath(getKey("dependencySources")),
      semanticdbRoot = getKeyOpt("semanticdbRoot").map(AbsolutePath(_))
    )
  }

  def fromDirectory(directory: AbsolutePath): InputProperties =
    InputProperties(
      directory,
      List(directory),
      None,
      Classpath(Nil),
      Classpath(Nil),
      None
    )
}
