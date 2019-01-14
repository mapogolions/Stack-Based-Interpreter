import org.junit.Test
import org.junit.Assert._
import io.github.mapogolions.cs305.buffalo.Vals._
import io.github.mapogolions.cs305.buffalo.Commands._
import io.github.mapogolions.cs305.buffalo.Stack


class TestStack {
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
