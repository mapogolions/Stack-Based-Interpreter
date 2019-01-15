import org.junit.Test
import org.junit.Assert._
import io.github.mapogolions.cs305.buffalo.Vals._
import io.github.mapogolions.cs305.buffalo.Commands._
import io.github.mapogolions.cs305.buffalo.Stack
import io.github.mapogolions.cs305.buffalo.{ Env, Scope, Empty }


class TestStack {
  @Test def TestBindStack: Unit = {
    /** 
      * Shared reference (the different scope)
      * let b = BOOL(true)
      * {
          let a = b
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
    assertEquals(
      Stack(STR("one") :: STR("two") :: BOOL(false) :: Nil).cond,
      Stack(STR("one") :: Nil)
    )
    assertEquals(
      Stack(STR("one") :: STR("two") :: BOOL(true) :: Nil).cond,
      Stack(STR("two") :: Nil)
    )
    assertEquals(
      Stack(STR("one") :: UNIT :: Nil).cond,
      Stack(ERROR :: STR("one") :: UNIT :: Nil)
    )
    assertEquals(
      Stack(STR("one") :: Nil).cond,
      Stack(ERROR :: STR("one") :: Nil)
    )
    assertEquals(
      Stack(Nil).cond,
      Stack(ERROR :: Nil)
    )
  }

  @Test def TestLessThanStack: Unit = {
    assertEquals(
      Stack(UNIT :: ID("name") :: Nil).lessThan,
      Stack(ERROR :: UNIT :: ID("name") :: Nil)
    )
    assertEquals(
      Stack(Nil).lessThan,
      Stack(ERROR :: Nil)
    )
    assertEquals(
      Stack(INT(7) :: INT(7) :: Nil).lessThan,
      Stack(BOOL(false) :: Nil)
    )
    assertEquals(
      Stack(INT(8) :: INT(7) :: Nil).lessThan,
      Stack(BOOL(true) :: Nil)
    )
  }

  @Test def TestEqualityStack: Unit = {
    assertEquals(
      Stack(Nil).equality,
      Stack(ERROR:: Nil)
    )
    assertEquals(
      Stack(INT(-1) :: Nil).equality,
      Stack(ERROR :: INT(-1) :: Nil)
    )
    assertEquals(
      Stack(INT(-1) :: INT(1) :: Nil).equality,
      Stack(BOOL(false) :: Nil)
    )
    assertEquals(
      Stack(INT(-1) :: INT(-1) :: Nil).equality,
      Stack(BOOL(true) :: Nil)
    )
  }

  @Test def TestNotStack: Unit = {
    assertEquals(
      Stack(BOOL(false) :: Nil).not,
      Stack(BOOL(true) :: Nil)
    )
    assertEquals(
      Stack(BOOL(true) :: Nil).not,
      Stack(BOOL(false) :: Nil)
    )
    assertEquals(
      Stack(ID("response") :: BOOL(true) :: Nil).not,
      Stack(ERROR :: ID("response") :: BOOL(true) :: Nil)
    )

    assertEquals(
      Stack(Nil).not,
      Stack(ERROR :: Nil)
    )
  }

  @Test def TestOrStack: Unit = {
    assertEquals(
      Stack(UNIT :: BOOL(true) :: Nil).or,
      Stack(ERROR :: UNIT :: BOOL(true) :: Nil)
    )
    assertEquals(
      Stack(BOOL(true) :: Nil).or,
      Stack(ERROR :: BOOL(true) :: Nil)
    )
    assertEquals(
      Stack(BOOL(true) :: BOOL(true) :: Nil).or,
      Stack(BOOL(true) :: Nil)
    )
    assertEquals(
      Stack(BOOL(true) :: BOOL(false) :: Nil).or,
      Stack(BOOL(true) :: Nil)
    )
  }

  @Test def TestAndStack: Unit = {
    assertEquals(
      Stack(UNIT :: BOOL(true) :: Nil).and,
      Stack(ERROR :: UNIT :: BOOL(true) :: Nil)
    )
    assertEquals(
      Stack(BOOL(true) :: Nil).and,
      Stack(ERROR :: BOOL(true) :: Nil)
    )
    assertEquals(
      Stack(BOOL(true) :: BOOL(true) :: Nil).and,
      Stack(BOOL(true) :: Nil)
    )
    assertEquals(
      Stack(BOOL(true) :: BOOL(false) :: Nil).and,
      Stack(BOOL(false) :: Nil)
    )
  }

  @Test def TestSwapStack: Unit = {
    assertEquals(
      Stack(ID("user") :: UNIT :: Nil).swap,
      Stack(UNIT :: ID("user") :: Nil)
    )
    assertEquals(
      Stack(Nil).swap,
      Stack(ERROR :: Nil)
    )
    assertEquals(
      Stack(UNIT:: Nil).swap,
      Stack(ERROR :: UNIT :: Nil)
    )
    assertEquals(
      Stack(BOOL(true) :: BOOL(false) :: Nil).swap,
      Stack(BOOL(false) :: BOOL(true) :: Nil)
    )
  }

  @Test def TestNegStack: Unit = {
    assertEquals(
      Stack(UNIT :: Nil).neg,
      Stack(List(ERROR, UNIT))
    )
    assertEquals(
      Stack(Nil).neg,
      Stack(List(ERROR))
    )
    assertEquals(
      Stack(List(INT(-10))).neg,
      Stack(List(INT(10)))
    )
    assertEquals(
      Stack(List(INT(0))).neg,
      Stack(List(INT(0)))
    )
    assertEquals(
      Stack(List(INT(10))).neg,
      Stack(List(INT(-10)))
    )
  }

  @Test def TestRemStack: Unit = {
    assertEquals(
      Stack(List(INT(5), INT(8))).rem,
      Stack(List(INT(3)))
    )
    assertEquals(
      Stack(List(INT(8), INT(5))).rem,
      Stack(List(INT(5)))
    )
    assertEquals(
      Stack(List(INT(9))).rem,
      Stack(List(ERROR, INT(9)))
    )
    assertEquals(
      Stack(List(INT(0), INT(2))).rem,
      Stack(List(ERROR, INT(0), INT(2)))
    )
    assertEquals(
      Stack(List(INT(2), INT(0), UNIT)).rem,
      Stack(List(INT(0), UNIT))
    )
  }

  @Test def TestDivStack: Unit = {
    assertEquals(
      Stack(List(INT(0), BOOL(true))).div,
      Stack( List(ERROR, INT(0), BOOL(true)) )
    )
    assertEquals(
      Stack(List(INT(-1), INT(0))).div,
      Stack( List(INT(0)) )
    )
    assertEquals(
      Stack(List(INT(5), INT(7))).div,
      Stack( List(INT(1)) )
    )
    assertEquals(
      Stack(List(INT(8), INT(5))).div,
      Stack( List(INT(0)) )
    )
  }
  @Test def TestMulStack: Unit = {
    assertEquals(
      Stack(Nil).mul,
      Stack( List(ERROR) )
    )
    assertEquals(
      Stack(List(INT(0), BOOL(true))).mul,
      Stack( List(ERROR, INT(0), BOOL(true)) )
    )
    assertEquals(
      Stack(List(INT(0), INT(-1))).mul,
      Stack( List(INT(0)) )
    )
  }

  @Test def TestSubStack: Unit = {
    assertEquals(
      Stack(List(INT(8), INT(5))).sub,
      Stack( List(INT(-3)) )
    )

    assertEquals(
      Stack(List(INT(0), BOOL(true))).sub,
      Stack( List(ERROR, INT(0), BOOL(true)) )
    )
    assertEquals(
      Stack(List(INT(0), INT(-1))).sub,
      Stack( List(INT(-1)) )
    )
  }

  @Test def TestAddStack: Unit = {
    assertEquals(
      Stack( List(INT(-10), INT(-100), BOOL(false)) ).add,
      Stack( List(INT(-110), BOOL(false)) )
    )
    assertEquals(
      Stack( List(INT(-10), INT(20)) ).add,
      Stack( List(INT(10)) )
    )
    assertEquals(
      Stack( List(INT(-10), BOOL(true)) ).add,
      Stack( List(ERROR, INT(-10), BOOL(true)) )
    )
    assertEquals(
      Stack(List(ERROR, UNIT)).add,
      Stack( List(ERROR, ERROR, UNIT) )
    )
    assertEquals(
      Stack(List()).add,
      Stack(List(ERROR))
    )
  }
}
