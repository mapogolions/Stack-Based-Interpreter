import org.junit.Test
import org.junit.Assert._
import io.github.mapogolions.cs305.buffalo.Vals._
import io.github.mapogolions.cs305.buffalo.Commands._
import io.github.mapogolions.cs305.buffalo.Parse
import io.github.mapogolions.cs305.buffalo.Stack
import io.github.mapogolions.cs305.buffalo.{ Scope, Empty }
import io.github.mapogolions.cs305.buffalo.Main


class TestInterpreterFunction {
  @Test def TestExecInterpreter: Unit = {
    assertEquals(
      Main.exec(
        Parse.commands(
          List(
            "push a",
            "push 10",
            "bind",
            "fun inc n",
            "push a",
            "push n",
            "add",
            "return",
            "funEnd",
            "push 2",
            "push inc",
            "call"
          )
        ),
        Stack() :: Nil,
        Empty
      ),
      (
        Stack(INT(12):: UNIT::UNIT::Nil) :: Nil,
        Scope(
          Map(
            "a" -> INT(10),
            "inc" -> CLOSURE(
              "inc",
              "n",
              PUSH(ID("a")) :: PUSH(ID("n")) :: ADD :: RETURN :: Nil,
              Scope(Map("a" -> INT(10)), Empty)
            )
          ),
          Empty
        )
      )
    )

    assertEquals(
      Main.exec(
        Parse.commands(
          List(
            "fun inc n",
            ":false:",
            "funEnd"
          )
        ),
        Stack() :: Nil,
        Empty
      ),
      (
        Stack(UNIT :: Nil) :: Nil,
        Scope(Map("inc" -> CLOSURE("inc", "n", PUSH(BOOL(false)) :: Nil, Empty)), Empty),
      )
    )
  }
}
