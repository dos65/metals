package scala.meta.internal.pc

import java.io.File
import java.net.URI
import javax.tools.JavaFileObject.Kind
import javax.tools.SimpleJavaFileObject

class SourceJavaFileObject(src: String, uri: URI, kind: Kind)
    extends SimpleJavaFileObject(uri, kind) {
  override def getCharContent(ignoreEncodingErrors: Boolean): CharSequence = src
}

object SourceJavaFileObject {
  def make(code: String): SourceJavaFileObject = {
    val out = new StringBuilder()
    out.append(code)

    val tmpFile = File.createTempFile("code-", ".java")
    new SourceJavaFileObject(code, URI.create(tmpFile.getPath), Kind.SOURCE)
  }
}
