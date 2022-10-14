package tests.interactive

import tests.BaseLspSuite
import munit.Assertions._

class InteractiveCompilationSuite extends BaseLspSuite("interactive-comp") {

  test("interactive-base") {
    cleanWorkspace()
    for {
      _ <- initialize(
        """|/metals.json
           |{
           |  "a": {"scalaVersion": "3.2.1-RC1-bin-SNAPSHOT"},
           |  "b": {
           |    "scalaVersion": "3.2.1-RC1-bin-SNAPSHOT",
           |    "dependsOn": [ "a" ]
           |  }
           |}
           |/a/src/main/scala/sample/A.scala
           |package sample
           |
           |object A:
           |  def foo: Int = "asdsdd"
           |  def bar: String = "asdsad"
           |/b/src/main/scala/sample/B.scala
           |package sample
           |
           |object B:
           |  A.bar
           |  A.foo
           |  A.unknown
           |""".stripMargin
      )
      _ <- server.didOpen("b/src/main/scala/sample/B.scala")
      // Here we check that there is an error in `a`
      // but due to inverative mode `b` was copiled too and `A.foo` and `A.bar` are valid members
      //  The only error in `b` is a reference to not defined member
      _ = assertNoDiff(
        client.workspaceDiagnostics,
        """|a/src/main/scala/sample/A.scala:4:18: error: Found:    ("asdsdd" : String)
           |Required: Int
           |  def foo: Int = "asdsdd"
           |                 ^^^^^^^^
           |b/src/main/scala/sample/B.scala:6:3: error: value unknown is not a member of object sample.A
           |  A.unknown
           |  ^^^^^^^^^
           |""".stripMargin,
      )
      _ = println()
    } yield ()

  }
}
