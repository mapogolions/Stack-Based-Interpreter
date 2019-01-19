import org.junit.Test
import org.junit.Assert._
import io.github.mapogolions.cs305.buffalo.Commands._
import io.github.mapogolions.cs305.buffalo.Vals._
import io.github.mapogolions.cs305.buffalo.Parse
import io.github.mapogolions.cs305.buffalo.Stack
import io.github.mapogolions.cs305.buffalo.{ Scope, Empty }
import io.github.mapogolions.cs305.buffalo.Main


class TestLetExpression {
  @Test def TestFunEnd: Unit = {
    assertEquals(
      Main.funEnd(
        List(
          FUN,
          PUSH(BOOL(true)),
          FUN,
          FUN,
          ADD,
          FUNEND,
          PUSH(STR("text")),
          FUNEND,
          FUNEND
        )
      ),
      (
        List(PUSH(BOOL(true)), FUN, FUN, ADD, FUNEND, PUSH(STR("text")), FUNEND),
        Nil
      )
    )
    assertEquals(
      Main.funEnd(FUN :: FUN :: FUNEND :: FUN :: FUNEND :: FUNEND :: Nil),
      (FUN :: FUNEND :: FUN :: FUNEND :: Nil) -> Nil
    )
    assertEquals(
      Main.funEnd(FUN :: FUN :: FUNEND :: FUNEND :: Nil),
      (FUN :: FUNEND :: Nil) -> Nil
    )
    assertEquals(
      Main.funEnd(FUN :: PUSH(BOOL(false)) :: FUNEND :: Nil),
      (PUSH(BOOL(false)) :: Nil) -> Nil
    )
    assertEquals(
      Main.funEnd(FUN :: FUNEND :: Nil),
      Nil -> Nil
    )
    assertEquals(
      Main.funEnd(FUN :: Nil),
      Nil -> Nil
    )
  }

  @Test def TestLetEnd: Unit = {
    assertEquals(
      Main.letEnd(
        List(
          LET,
          PUSH(BOOL(true)),
          PUSH(INT(10)),
          END,
          LET,
          SWAP,
          END
        )
      ),
      (
        PUSH(BOOL(true)) :: PUSH(INT(10)) :: Nil,
        LET :: SWAP :: END :: Nil
      )
    )
    assertEquals(
      Main.letEnd(
        List(
          LET,
          PUSH(BOOL(true)),
          LET,
          LET,
          ADD,
          END,
          PUSH(STR("text")),
          END,
          END
        )
      ),
      (
        List(PUSH(BOOL(true)), LET, LET, ADD, END, PUSH(STR("text")), END),
        Nil
      )
    )
    assertEquals(
      Main.letEnd(LET :: LET :: END :: LET :: END :: END :: Nil),
      (LET :: END :: LET :: END :: Nil) -> Nil
    )
    assertEquals(
      Main.letEnd(LET :: LET :: END :: END :: Nil),
      (LET :: END :: Nil) -> Nil
    )
    assertEquals(
      Main.letEnd(LET :: PUSH(BOOL(false)) :: END :: Nil),
      (PUSH(BOOL(false)) :: Nil) -> Nil
    )
    assertEquals(
      Main.letEnd(LET :: END :: Nil),
      Nil -> Nil
    )
    assertEquals(
      Main.letEnd(LET :: Nil),
      Nil -> Nil
    )
  }

  @Test def TestFunEndBalanced: Unit = {
    assertTrue(Main.funEndBalanced(FUN :: FUN :: FUNEND :: FUN:: FUNEND :: FUNEND :: Nil))
    assertTrue(Main.funEndBalanced(FUN :: FUN :: FUNEND :: FUNEND :: Nil))
    assertFalse(Main.funEndBalanced(FUN :: FUN :: FUNEND :: Nil))
  }

  @Test def TestLetEndBalanced: Unit = {
    assertTrue(Main.letEndBalanced(LET :: LET :: END :: LET:: END :: END :: Nil))
    assertTrue(Main.letEndBalanced(LET :: LET :: END :: END :: Nil))
    assertFalse(Main.letEndBalanced(LET :: LET :: Nil))
  }
}
