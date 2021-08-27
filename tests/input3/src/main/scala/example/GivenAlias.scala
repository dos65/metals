package example

given intValue: Int = 4
given String = "str"

def method(using Int) = ""

val anonUsage = given_String

object X {
  given Double = 4.0
  val double = given_Double
}

trait Z[A]:
  def doZ: List[A]

given [T]: Z[T] with 
  def doZ: List[T] = Nil
