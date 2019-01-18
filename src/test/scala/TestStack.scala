import org.junit.Test
import org.junit.Assert._
import io.github.mapogolions.cs305.buffalo.Vals._
import io.github.mapogolions.cs305.buffalo.Commands._
import io.github.mapogolions.cs305.buffalo.Stack
import io.github.mapogolions.cs305.buffalo.{ Env, Scope, Empty }


class TestStack {
  @Test def TestCallFunctionStack: Unit = {
    assertEquals(
      Stack(ID("inc") :: INT(10) :: Nil).callFunc(
        Scope(
          Map("inc" -> CLOSURE("inc", "n", PUSH(BOOL(true)) :: Nil, Empty)),
          Empty
        )
      ),
      (
        PUSH(BOOL(true)) :: Nil, // body function
        Stack(Nil),
        Scope(                   // funtion scope + recur
          Map("n" -> INT(10)),
          Scope(
            Map("inc" -> CLOSURE("inc", "n", PUSH(BOOL(true)) :: Nil, Empty)),
            Empty
          )
        )
      )
    )
    // call unbound function -> ERROR
    assertEquals(
      Stack(ID("inc") :: INT(10) :: Nil).callFunc(Empty),
      (Nil, Stack(ERROR :: ID("inc") :: INT(10) :: Nil), Empty)
    )
  }

  @Test def TestBindStack: Unit = {
    /**
      * Shared reference (the different scope)
      * let b = BOOL(true)
      * {
      *  let a = b
      * }
      */
    assertEquals(
      Stack(ID("a") :: ID("a") :: Nil).bind(
        Scope(
          Map(),
          Scope(
            Map("a" -> BOOL(true)),
            Empty
          )
        )
      ),
      (
        Stack(UNIT :: Nil),
        Scope(
          Map("a" -> BOOL(true)),
          Scope(
            Map("a" -> BOOL(true)),
            Empty
          )
        )
      )
    )

    /**
      * Shared reference (the same scope)
      * let b = INT(10)
      * let a = b
      */
    assertEquals(
      Stack(ID("a") :: ID("b") :: Nil).bind(
        Scope(
          Map("a" -> INT(10)),
          Empty
        )
      ),
      ( Stack(UNIT :: Nil), Scope(Map("a" -> INT(10), "b" -> INT(10)), Empty) )
    )

    /** Different scope (shade)
      * let a = BOOL(true)
      * {
      *   let a = STR("hello")
      * }
      */
    assertEquals(
      Stack(STR("hello") :: ID("a") :: Nil)
        .bind(
          Scope(
            Map(),
            Scope(
              Map("a" -> BOOL(true)),
              Empty
            )
          )
        ),
      (
        Stack(UNIT :: Nil),
        Scope(
          Map("a" -> STR("hello")),
          Scope(
            Map("a" -> BOOL(true)),
            Empty
          )
        )
      )
    )

    /** The same cope (rewrite)
      * let b = UNIT
      * let b = BOOL(false)
     */
    assertEquals(
      Stack(BOOL(false) :: ID("b") :: Nil).bind(Scope(Map("b" -> UNIT), Empty)),
      ( Stack(UNIT :: Nil), Scope(Map("b" -> BOOL(false)), Empty) )
    )
    assertEquals(
      Stack(INT(10) :: ID("a") :: Nil).bind(Empty),
      ( Stack(UNIT :: Nil), Scope(Map("a" -> INT(10)), Empty) )
    )
    assertEquals(
      Stack(ERROR :: BOOL(false) :: Nil).bind(Empty),
      ( Stack(ERROR :: ERROR :: BOOL(false) :: Nil), Empty )
    )
  }

  @Test def TestIfStack: Unit = {
    val env = Scope(
      Map("a" -> BOOL(true)),
      Scope(
        Map("b" -> BOOL(false)),
        Empty
      )
    )
    assertEquals(
      Stack(STR("one") :: STR("two") :: ID("b") :: Nil).cond(env),
      Stack(STR("two") :: Nil) -> env
    )
    assertEquals(
      Stack(STR("one") :: STR("two") :: ID("a") :: Nil).cond(env),
      Stack(STR("one") :: Nil) -> env
    )
    assertEquals(
      Stack(STR("one") :: UNIT :: Nil).cond(env),
      Stack(ERROR :: STR("one") :: UNIT :: Nil) -> env
    )
  }

  @Test def TestLessThanStack: Unit = {
    val env = Scope(
      Map("a" -> INT(10)),
      Scope(
        Map("b" -> INT(10)),
        Scope(
          Map("c" -> INT(2)),
          Empty
        )
      )
    )
    assertEquals(
      Stack(ID("a") :: ID("b") :: STR(".") :: Nil).lessThan(env),
      Stack(BOOL(false) :: STR(".") :: Nil) -> env
    )
    assertEquals(
      Stack(INT(-1) :: ID("d"):: Nil).lessThan(env),
      Stack(ERROR :: INT(-1) :: ID("d") :: Nil) -> env
    )
    assertEquals(
      Stack(ID("a") :: ID("c") :: Nil).lessThan(env),
      Stack(BOOL(true) :: Nil) -> env
    )
  }

  @Test def TestEqualityStack: Unit = {
    val env = Scope(
      Map("a" -> INT(10)),
      Scope(
        Map("b" -> INT(10)),
        Scope(
          Map("c" -> INT(2)),
          Empty
        )
      )
    )
    assertEquals(
      Stack(ID("a") :: ID("b") :: STR(".") :: Nil).equality(env),
      Stack(BOOL(true) :: STR(".") :: Nil) -> env
    )
    assertEquals(
      Stack(INT(-1) :: ID("d"):: Nil).equality(env),
      Stack(ERROR :: INT(-1) :: ID("d") :: Nil) -> env
    )
    assertEquals(
      Stack(ID("a") :: ID("c") :: Nil).equality(env),
      Stack(BOOL(false) :: Nil) -> env
    )
  }

  @Test def TestNotStack: Unit = {
    val env = Scope(
      Map("a" -> BOOL(true)),
      Scope(
        Map("b" -> STR("hello world")),
        Scope(
          Map("c" -> BOOL(false)),
          Empty
        )
      )
    )
    assertEquals(
      Stack(BOOL(false) :: Nil).not(env),
      Stack(BOOL(true) :: Nil) -> env
    )
    assertEquals(
      Stack(ID("a") :: STR("text"):: Nil).not(env),
      Stack(BOOL(false) :: STR("text"):: Nil) -> env
    )
    assertEquals(
      Stack(ID("b"):: Nil).not(env),
      Stack(ERROR :: ID("b") :: Nil) -> env
    )
  }

  @Test def TestOrStack: Unit = {
    val env = Scope(
      Map("a" -> BOOL(true)),
      Scope(
        Map("b" -> STR("hello world")),
        Scope(
          Map("c" -> BOOL(false)),
          Empty
        )
      )
    )
    assertEquals(
      Stack(ID("c") :: BOOL(true) :: Nil).or(env),
      Stack(BOOL(true) :: Nil) -> env
    )
    assertEquals(
      Stack(BOOL(false) :: ID("a"):: Nil).or(env),
      Stack(BOOL(true) :: Nil) -> env
    )
  }

  @Test def TestAndStack: Unit = {
    val env = Scope(
      Map("a" -> BOOL(true)),
      Scope(
        Map("b" -> STR("hello world")),
        Scope(
          Map("c" -> BOOL(false)),
          Empty
        )
      )
    )
    assertEquals(
      Stack(ID("a") :: ID("c"):: Nil).and(env),
      Stack(BOOL(false) :: Nil) -> env
    )
    assertEquals(
      Stack(ID("a") :: BOOL(true):: Nil).and(env),
      Stack(BOOL(true) :: Nil) -> env
    )
    assertEquals(
      Stack(ID("a") :: ID("b"):: Nil).and(env),
      Stack(ERROR :: ID("a") :: ID("b"):: Nil) -> env
    )
  }

  @Test def TestSwapStack: Unit = {
    assertEquals(
      Stack(ID("user") :: UNIT :: Nil).swap(Empty),
      Stack(UNIT :: ID("user") :: Nil) -> Empty
    )
    assertEquals(
      Stack(Nil).swap(Empty),
      Stack(ERROR :: Nil) -> Empty
    )
    assertEquals(
      Stack(UNIT:: Nil).swap(Empty),
      Stack(ERROR :: UNIT :: Nil) -> Empty
    )
    assertEquals(
      Stack(BOOL(true) :: BOOL(false) :: Nil).swap(Empty),
      Stack(BOOL(false) :: BOOL(true) :: Nil) -> Empty
    )
  }

 @Test def TestNegStack: Unit = {
    val env = Scope(
      Map("a" -> INT(10)),
      Scope(
        Map("b" -> INT(5)),
        Scope(
          Map("c" -> INT(0)),
          Empty
        )
      )
    )
    assertEquals(
      Stack(UNIT :: Nil).neg(env),
      Stack(ERROR :: UNIT :: Nil) -> env
    )
    assertEquals(
      Stack(Nil).neg(env),
      Stack(ERROR :: Nil) -> env
    )
    assertEquals(
      Stack(INT(-10) :: Nil).neg(env),
      Stack(INT(10) :: Nil) -> env
    )
    assertEquals(
      Stack(ID("a") :: Nil).neg(env),
      Stack(INT(-10) :: Nil) -> env
    )
    assertEquals(
      Stack(ID("c") :: Nil).neg(env),
      Stack(INT(0) :: Nil) -> env
    )
  }

  @Test def TestRemStack: Unit = {
     val env = Scope(
      Map("a" -> INT(10)),
      Scope(
        Map("b" -> INT(5)),
        Scope(
          Map("c" -> INT(0)),
          Empty
        )
      )
    )
    assertEquals(
      Stack(INT(5) :: INT(8) :: Nil).rem(Empty),
      Stack(INT(3) :: Nil) -> Empty
    )
    assertEquals(
      Stack(ID("c") :: INT(7):: Nil).rem(env),
      Stack(ERROR :: ID("c") :: INT(7) :: Nil) -> env
    )
    assertEquals(
      Stack(ID("b") :: INT(7):: Nil).rem(env),
      Stack(INT(2) :: Nil) -> env
    )
  }

  @Test def TestDivStack: Unit = {
    val env = Scope(
      Map("a" -> INT(10)),
      Scope(
        Map("b" -> INT(5)),
        Scope(
          Map("c" -> INT(0)),
          Empty
        )
      )
    )
    // division by zero
    assertEquals(
      Stack(ID("c") :: ID("b") :: Nil).div(env),
      Stack(ERROR :: ID("c") :: ID("b") :: Nil) -> env
    )
    assertEquals(
      Stack(ID("b") :: ID("a") :: Nil).div(env),
      Stack(INT(2) :: Nil) -> env
    )
    assertEquals(
      Stack(INT(5) :: INT(10):: Nil).div(Empty),
      Stack(INT(2) :: Nil) -> Empty
    )
    assertEquals(
      Stack(INT(0) :: INT(10):: Nil).div(Empty),
      Stack(ERROR :: INT(0) :: INT(10) :: Nil) -> Empty
    )
  }

  @Test def TestMulStack: Unit = {
    assertEquals(
      Stack(Nil).mul(Empty),
      Stack(ERROR :: Nil) -> Empty
    )
    assertEquals(
      Stack(ID("in") :: ID("out") :: Nil)
        .mul(Scope(Map("in" -> INT(10)), Scope(Map("out" -> BOOL(false)), Empty))),
      (
        Stack(ERROR :: ID("in") :: ID("out") :: Nil),
        Scope(Map("in" -> INT(10)), Scope(Map("out" -> BOOL(false)), Empty))
      )
    )
  }

  @Test def TestSubStack: Unit = {
    assertEquals(
      Stack(ID("a") :: ID("b") :: Nil).sub(Scope(Map("a" -> INT(10), "b" -> INT(-15)), Empty)),
      Stack(INT(-25) :: Nil) -> Scope(Map("a" -> INT(10), "b" -> INT(-15)), Empty)
    )
    assertEquals(
      Stack(ID("in") :: ID("out") :: Nil)
        .sub(Scope(Map("in" -> INT(10)), Scope(Map("out" -> INT(-15)), Empty))),
      Stack(INT(-25) :: Nil) -> Scope(Map("in" -> INT(10)), Scope(Map("out" -> INT(-15)), Empty))
    )
  }

  @Test def TestAddStack: Unit = {
    assertEquals(
      Stack(INT(-10) :: INT(-100) :: BOOL(false) :: Nil ).add(Empty),
      Stack( List(INT(-110), BOOL(false))) -> Empty
    )
    assertEquals(
      Stack(INT(-10) :: ID("a") :: Nil).add(Scope(Map("a" -> INT(-23)), Empty)),
      Stack(INT(-33) :: Nil) -> Scope(Map("a" -> INT(-23)), Empty)
    )
    assertEquals(
      Stack(ID("b") :: INT(-1) :: Nil ).add(Scope(Map("b" -> INT(34)), Empty)),
      Stack(INT(33) :: Nil) -> Scope(Map("b" -> INT(34)), Empty)
    )
    assertEquals(
      Stack(ERROR :: UNIT :: Nil).add(Empty),
      Stack(ERROR :: ERROR :: UNIT :: Nil) -> Empty
    )
  }
}
