package scala.meta.internal.metals

class SharedClassLoader(
    sharedPrefixes: List[String],
    parent: ClassLoader
) extends ClassLoader(null) {

  override def findClass(name: String): Class[_] = {
    val isShared = sharedPrefixes.exists(prefix => name.startsWith(prefix))
    if (isShared) {
      parent.loadClass(name)
    } else {
      throw new ClassNotFoundException(name)
    }
  }
}

object SharedClassLoader {

  def create(
      sharedPrefixes: List[String],
      parent: ClassLoader
  ): ClassLoader =
    new SharedClassLoader(sharedPrefixes, parent)

  def forTastyInspect(parent: ClassLoader): ClassLoader =
    create(List("scala.meta.scala3"), parent)

}
