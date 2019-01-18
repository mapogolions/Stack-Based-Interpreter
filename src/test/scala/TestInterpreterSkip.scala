import org.junit.Test
import org.junit.Assert._
import io.github.mapogolions.cs305.buffalo.Vals._
import io.github.mapogolions.cs305.buffalo.Commands._
import io.github.mapogolions.cs305.buffalo.Parse
import io.github.mapogolions.cs305.buffalo.Stack
import io.github.mapogolions.cs305.buffalo.{ Scope, Empty }
import io.github.mapogolions.cs305.buffalo.Main


class TestInterpreterSkip {
  @Test def TestExecInterpreter = {
    assertEquals(
      Main.skip(
        List(
          PUSH(BOOL(true)),
          PUSH(ID("b")),
          FUNEND,
          PUSH(STR("hello")),
          PUSH(BOOL(false))
        ),
        "inc",
        "n",
        Empty
      ),
      (
        PUSH(UNIT) :: PUSH(STR("hello")) :: PUSH(BOOL(false)) :: Nil,
        Scope(
          Map("inc" -> CLOSURE("inc", "n", PUSH(BOOL(true)) :: PUSH(ID("b")):: Nil, Empty)),
          Empty
        )
      )
    )

    assertEquals(
      Main.skip(Nil, "inc", "n", Empty),
      Nil -> Empty
    )
  }
}
