import org.junit.Test
import org.junit.Assert._
import io.github.mapogolions.cs305.buffalo.{ Ops, Vals, Commands, Stack }

class TestStack {
  import Vals._
  import Commands._

  @Test def TestRemStack: Unit = {
    assertEquals(
      Stack.rem(List(INT(5), INT(8))),List(INT(3))
    )
    assertEquals(
      Stack.rem(List(INT(8), INT(5))),List(INT(5))
    )
    assertEquals(
      Stack.rem(List(INT(9))), List(ERROR, INT(9))
    )
    assertEquals(
      Stack.rem(List(INT(0), INT(2))), List(ERROR, INT(0), INT(2))
    )
    assertEquals(
      Stack.rem(List(INT(2), INT(0), UNIT)), List(INT(0), UNIT)
    )
  }

  @Test def TestDivStack: Unit = {
    assertEquals(
      Stack.div(List(INT(0), BOOL(true))), List(ERROR, INT(0), BOOL(true))
    )
    assertEquals(
      Stack.div(List(INT(0), INT(-1))), List(ERROR, INT(0), INT(-1))
    )
    assertEquals(
      Stack.div(List(INT(-1), INT(0))), List(INT(0))
    )
    assertEquals(
      Stack.div(List(INT(5), INT(7))), List(INT(1))
    )
    assertEquals(
      Stack.div(List(INT(8), INT(5))), List(INT(0))
    )
  }
  @Test def TestMulStack: Unit = {
    assertEquals(Stack.mul(Nil), List(ERROR))
    assertEquals(
      Stack.mul(List(INT(0), BOOL(true))), List(ERROR, INT(0), BOOL(true))
    )
    assertEquals(
      Stack.mul(List(INT(0), INT(-1))), List(INT(0))
    )
  }

  @Test def TestSubStack: Unit = {
    assertEquals(
      Stack.sub(List(INT(8), INT(5))), List(INT(-3))
    )

    assertEquals(
      Stack.sub(List(INT(0), BOOL(true))), List(ERROR, INT(0), BOOL(true))
    )
    assertEquals(
      Stack.sub(List(INT(0), INT(-1))), List(INT(-1))
    )
  }

  @Test def TestAddStack: Unit = {
    assertEquals(
      Stack.add(List(INT(-10), INT(-100), BOOL(false))), List(INT(-110), BOOL(false))
    )
    assertEquals(
      Stack.add(List(INT(-10), INT(20))), List(INT(10))
    )
    assertEquals(
      Stack.add(List(INT(-10), BOOL(true))), List(ERROR, INT(-10), BOOL(true))
    )
    assertEquals(
      Stack.add(List(ERROR, UNIT)), List(ERROR, ERROR, UNIT)
    )
    assertEquals(Stack.add(List()), List(ERROR))
  }
}
