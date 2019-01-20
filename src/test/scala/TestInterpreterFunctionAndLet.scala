import org.junit.Test
import org.junit.Assert._
import io.github.mapogolions.cs305.buffalo.Vals._
import io.github.mapogolions.cs305.buffalo.Commands._
import io.github.mapogolions.cs305.buffalo.Parse
import io.github.mapogolions.cs305.buffalo.Stack
import io.github.mapogolions.cs305.buffalo.{ Scope, Empty }
import io.github.mapogolions.cs305.buffalo.Main


class TestInterpreterFunctionsAndLet {
  @Test def TestExecInterpreter: Unit = {
    // Example 4 - Scope chain
    assertEquals(
      Main.exec(
        Parse.commands(
          List(
            "push y",
            "push 5",
            "bind",

            "let",
              "push y",
              "push 7",
              "bind",

              "fun addY x",
                "let",
                  "push x",
                  "push y",
                  "add",
                "end",
                "return",
              "funEnd",

              "push 2",
              "push addY",
              "call",
            "end"
          )
        ),
        Stack(),
        Empty
      ),
      (
        Stack(INT(9)::UNIT::Nil),
        Scope(Map("y" -> INT(5)), Empty)
      )
    )

    // Example 3
    assertEquals(
      Main.exec(
        Parse.commands(
          List(
            "fun double x",
              "let",
                "push x",
                "push x",
                "add",
              "end",
              "return",
            "funEnd",
            "push 2",
            "push double",
            "call",
            "quit"
          )
        ),
        Stack(),
        Empty
      ),
      (
        Stack(INT(4)::UNIT::Nil),
        Scope(
          Map(
            "double" -> CLOSURE(
              "double",
              "x",
              LET::PUSH(ID("x"))::PUSH(ID("x"))::ADD::END::RETURN::Nil,
              Empty
            )
          ),
          Empty
        )
      )
    )

    // Example 2
    assertEquals(
      Main.exec(
        Parse.commands(
          List(
            "fun identity x",
              "let",
                "push x",
               "end",
               "return",
            "funEnd",
            "push 1",
            "push identity",
            "call",
            "quit"
          )
        ),
        Stack(),
        Empty
      ),
      (
        Stack(INT(1)::UNIT::Nil),
        Scope(
          Map(
            "identity" -> CLOSURE(
              "identity",
              "x",
              LET::PUSH(ID("x"))::END::RETURN::Nil,
              Empty
            )
          ),
          Empty
        )
      )
    )

    // Example 1
    assertEquals(
      Main.exec(
        Parse.commands(
          List(
            "let",
            "fun identity x",
              "push x",
              "return",
             "funEnd",
            "end",
            "push 1",
            "push identity",
            "call",
            "quit"
          )
        ),
        Stack(),
        Empty
      ),
      (
        Stack(ERROR::ID("identity")::INT(1)::UNIT::Nil),
        Empty
      )
    )
  }
}
