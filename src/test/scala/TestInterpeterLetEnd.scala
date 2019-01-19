import org.junit.Test
import org.junit.Assert._
import io.github.mapogolions.cs305.buffalo.Vals._
import io.github.mapogolions.cs305.buffalo.Parse
import io.github.mapogolions.cs305.buffalo.Stack
import io.github.mapogolions.cs305.buffalo.{ Scope, Empty }
import io.github.mapogolions.cs305.buffalo.Main


class TestInterpreterLetEnd {
  @Test def TestExecInterpreter = {
    assertEquals(
      Main.exec(
        Parse.commands(
          List(
            "let",
              "push 3",
              "push 10",
            "end",
            "add",
            "quit"
          )
        ),
        Stack(),
        Empty
      ),
      Stack(ERROR :: INT(10) :: Nil) -> Empty
    )
    assertEquals(
      Main.exec(
        Parse.commands(
          List(
            "let",
              "push 3",
            "end",
            "let",
              "push b",
              "swap",
              "bind",
            "end",
          )
        ),
        Stack(),
        Empty
      ),
      Stack(ERROR :: INT(3) :: Nil) -> Empty
    )
    assertEquals(
      Main.exec(
        Parse.commands(
          List(
            "let",
              "push a1",
              "push 7.2",
              "bind",
            "end",
            "quit"
          )
        ),
        Stack(),
        Empty
      ),
      Stack(ERROR :: Nil) -> Empty
    )
    assertEquals(
      Main.exec(
        Parse.commands(
          List(
            """push "harry"""",
              "let",
                "push 2",
                "push 3",
                "push 4",
              "end",
              "push 5"
          )
        ),
        Stack(),
        Empty
      ),
      Stack(INT(5) :: INT(4) :: STR("harry") :: Nil) -> Empty
    )
    assertEquals(
      Main.exec(
        Parse.commands(
          List(
            "let",
              "push c",
              "push 13",
              "bind",
              "let",
                "push a",
                "push 3",
                "bind",
                "push a",
                "push c",
                "add",
              "end",

              "let",
                "push b",
                """push "ron""""",
                "bind",
              "end",
            "end"
          )
        ),
        Stack(),
        Empty
      ),
      Stack(UNIT :: Nil) -> Empty
    )
    assertEquals(
      Main.exec(
        Parse.commands(
          List(
            "push a",
            "push 10",
            "bind",
            "let",
              "push b",
              "push -5",
              "bind",
              "push b",
              "push a",
              "sub",
            "end"
          )
        ),
        Stack(),
        Empty
      ),
      Stack(INT(-15) :: UNIT :: Nil) -> Scope(Map("a" -> INT(10)), Empty)
    )
    assertEquals(
      Main.exec(
        Parse.commands("push 4" :: "end" :: "push :false:" :: Nil),
        Stack(),
        Empty
      ),
      Stack(INT(4) :: Nil) -> Empty
    )
    assertEquals(
      Main.exec(
        Parse.commands("push 4" :: "let" :: "push :true:" :: "end" :: Nil),
        Stack(),
        Empty
      ),
      Stack(BOOL(true) :: INT(4) :: Nil) -> Empty
    )
    assertEquals(
      Main.exec(
        Parse.commands("push 4" :: "let" :: "push b" :: "push :true:" :: "bind" :: "end" :: Nil),
        Stack(),
        Empty
      ),
      Stack(UNIT :: INT(4) :: Nil) -> Empty
    )
    assertEquals(
      Main.exec(
        Parse.commands("push 4" :: "let" :: "end" :: Nil),
        Stack(),
        Empty
      ),
      Stack(UNIT :: INT(4) :: Nil) -> Empty
    )
  }
}