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
        Parse.commands("push 4" :: "end" :: "push :false:" :: Nil),
        Stack() :: Nil,
        Empty
      ),
      (Stack(BOOL(false):: ERROR :: INT(4) :: Nil) :: Nil) -> Empty
    )
    assertEquals(
      Main.exec(
        Parse.commands("push 4" :: "let" :: "push :true:" :: "end" :: Nil),
        Stack() :: Nil,
        Empty
      ),
      (Stack(BOOL(true) :: INT(4) :: Nil) :: Nil) -> Empty
    )
    assertEquals(
      Main.exec(
        Parse.commands("push 4" :: "let" :: "push b" :: "push :true:" :: "bind" :: "end" :: Nil),
        Stack() :: Nil,
        Empty
      ),
      (Stack(UNIT :: INT(4) :: Nil) :: Nil) -> Empty
    )
    assertEquals(
      Main.exec(
        Parse.commands("push 4" :: "let" :: "end" :: Nil),
        Stack() :: Nil,
        Empty
      ),
      (Stack(UNIT :: INT(4) :: Nil) :: Nil) -> Empty
    )
  }
}