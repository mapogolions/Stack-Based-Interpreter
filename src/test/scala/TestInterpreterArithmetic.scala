import org.junit.Test
import org.junit.Assert._
import io.github.mapogolions.cs305.buffalo.Vals._
import io.github.mapogolions.cs305.buffalo.Parse
import io.github.mapogolions.cs305.buffalo.Stack
import io.github.mapogolions.cs305.buffalo.{ Scope, Empty }
import io.github.mapogolions.cs305.buffalo.Main


class TestInterpreterArithmetic {
  @Test def TestExecInterpreter: Unit = {
    assertEquals(
      Main.exec(
        Parse.commands("push 2" :: "push 10" :: "sub" :: Nil),
        Stack() :: Nil,
        Empty
      ),
      (Stack(INT(-8) :: Nil) :: Nil) -> Empty
    )
    assertEquals(
      Main.exec(
        Parse.commands("push 2" :: "push 10" :: "mul" :: Nil),
        Stack() :: Nil,
        Empty
      ),
      (Stack(INT(20) :: Nil) :: Nil) -> Empty
    )
    assertEquals(
      Main.exec(
        Parse.commands("push 10" :: "push 10" :: "add" :: Nil),
        Stack() :: Nil,
        Empty
      ),
      (Stack(INT(20) :: Nil) :: Nil) -> Empty
    )
  }
}
