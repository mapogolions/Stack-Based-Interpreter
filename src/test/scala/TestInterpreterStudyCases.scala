import org.junit.Test
import org.junit.Assert._
import io.github.mapogolions.cs305.buffalo.Vals._
import io.github.mapogolions.cs305.buffalo.Parse
import io.github.mapogolions.cs305.buffalo.Stack
import io.github.mapogolions.cs305.buffalo.{ Scope, Empty }
import io.github.mapogolions.cs305.buffalo.Main


class TestInterpreterStudyCases {
  @Test def TestExecInterpreter = {
    assertEquals(
      Main.exec(
        Parse.commands(
          List(
            "push a",
            "push 9",
            "bind",
            "push a",
            "push 3",
            "quit"
          )
        ),
        Stack() :: Nil,
        Empty
      ),
      (
        Stack(INT(3) :: ID("a") :: UNIT :: Nil) :: Nil,
        Scope(Map("a" -> INT(9)), Empty)
      )
    )
    assertEquals(
      Main.exec(
        Parse.commands(
          List(
            ":true:",
            ":false:",
            "and",
            "not",
            ":false:",
            "or",
            "quit"
          )
        ),
        Stack() :: Nil,
        Empty
      ),
      (
        Stack(BOOL(true) :: Nil) :: Nil,
        Empty
      )
    )
    assertEquals(
      Main.exec(
        Parse.commands(
          List(
            ":true:",
            "push 3",
            "and",
            "push 5",
            "quit"
          )
        ),
        Stack() :: Nil,
        Empty
      ),
      (
        Stack(INT(5) :: ERROR :: INT(3) :: BOOL(true) :: Nil) :: Nil,
        Empty
      )
    )

    assertEquals(
      Main.exec(
        Parse.commands(
          List(
            "push a",
            "push 7",
            "push 9",
            "equal",
            "push 10",
            "push 17",
            "lessThan",
            "quit"
          )
        ),
        Stack() :: Nil,
        Empty
      ),
      (
        Stack(BOOL(true) :: BOOL(false) :: ID("a") :: Nil) :: Nil,
        Empty
      )
    )

    assertEquals(
      Main.exec(
        Parse.commands(
          List(
            "push num1",
            "push 8",
            "bind",
            "push num2",
            "push 9",
            "bind",
            "push num3",
            "quit"
          )
        ),
        Stack() :: Nil,
        Empty
      ),
      (
        Stack(ID("num3") :: UNIT :: UNIT :: Nil) :: Nil,
        Scope(Map("num2" -> INT(9), "num1" -> INT(8)), Empty)
      )
    )

    assertEquals(
      Main.exec(
        Parse.commands(
          List(
            "push a",
            "push 5",
            "bind",
            "push b",
            "push 10",
            "bind",
            "push a",
            "push b",
            "add",
            "push 3",
            "div",
            "quit"
          )
        ),
        Stack() :: Nil,
        Empty
      ),
      (
        Stack(INT(5) :: UNIT :: UNIT :: Nil) :: Nil,
        Scope(Map("a" -> INT(5), "b" -> INT(10)), Empty)
      )
    )

    assertEquals(
      Main.exec(
        Parse.commands(
          List(
            ":false:",
            ":true:",
            "push 11",
            "push 4",
            "if",
            "push 8",
            "if",
            "quit"
          )
        ),
        Stack() :: Nil,
        Empty
      ),
      (
        Stack(INT(4) :: Nil) :: Nil,
        Empty
      )
    )
  }
}