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
    /* assertEquals(
      Main.exec(
        Parse.commands(
          List(
            "fun stop arg",
              "push 1",
              "return",
            "funEnd",
            "fun factorial arg",
              "push arg",
              "push 1",
              "sub",
              "push 1",
              "push arg",
              "equal",
              "push factorial",
              "push stop",
              "if",
              "call",
              "push arg",
              "mul",
              "return",
            "funEnd",
            "push 3",
            "push factorial",
            "call",
            "quit"
          )
        ),
        Stack(),
        Empty
      ),
      (
        Stack(INT(6)::UNIT:: UNIT::UNIT::UNIT::Nil),
        Scope(
          Map(
            "x" -> INT(5),
            "a" -> INT(3),
            "addX" -> CLOSURE(
              "addX",
              "arg",
              PUSH(ID("x"))::PUSH(ID("arg"))::ADD:: RETURN::Nil,
              Scope(
                Map("x" -> INT(3)),
                Empty
              )
            )
          ),
          Empty
        )
      )
    ) */

    assertEquals(
      Main.exec(
        Parse.commands(
          List(
            "push x",
            "push 3",
            "bind",
            "fun addX arg",
              "push x",
              "push arg",
              "add",
              "return",
            "funEnd",
            "push x",
            "push 5",
            "bind",
            "push a",
            "push 3",
            "bind",
            "push a",
            "push addX",
            "call",
            "quit"
          )
        ),
        Stack(),
        Empty
      ),
      (
        Stack(INT(6)::UNIT:: UNIT::UNIT::UNIT::Nil),
        Scope(
          Map(
            "x" -> INT(5),
            "a" -> INT(3),
            "addX" -> CLOSURE(
              "addX",
              "arg",
              PUSH(ID("x"))::PUSH(ID("arg"))::ADD:: RETURN::Nil,
              Scope(
                Map("x" -> INT(3)),
                Empty
              )
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
            "fun identity x",
              "push x",
              "return",
            "funEnd",
            "push x",
            "push 1",
            "bind",
            "push x",
            "push identity",
            "call",
            "quit"
          )
        ),
        Stack(),
        Empty
      ),
      (
        Stack(INT(1) :: UNIT ::  UNIT :: Nil),
        Scope(
          Map(
            "x" -> INT(1),
            "identity" -> CLOSURE(
              "identity",
              "x",
              PUSH(ID("x")) ::  RETURN :: Nil,
              Empty
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
            "fun identity x",
            "push x",
            "return",
            "funEnd",
            "push 1.2",
            "push identity",
            "call",
            "quit"
          )
        ),
        Stack(),
        Empty
      ),
      (
        Stack(ERROR :: ID("identity") :: ERROR ::  UNIT ::Nil),
        Scope(
          Map(
            "identity" -> CLOSURE(
              "identity",
              "x",
              PUSH(ID("x")) ::  RETURN :: Nil,
              Empty
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
            "fun identity x",
              "push x",
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
        Stack(INT(1) :: UNIT ::Nil),
        Scope(
          Map(
            "identity" -> CLOSURE(
              "identity",
              "x",
              PUSH(ID("x")) ::  RETURN :: Nil,
              Empty
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
        Stack(),
        Empty
      ),
      (
        Stack(INT(12):: UNIT::UNIT::Nil),
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
        Stack(),
        Empty
      ),
      (
        Stack(UNIT :: Nil),
        Scope(Map("inc" -> CLOSURE("inc", "n", PUSH(BOOL(false)) :: Nil, Empty)), Empty),
      )
    )
  }
}
