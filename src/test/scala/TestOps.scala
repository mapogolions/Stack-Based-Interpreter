import org.junit.Test
import org.junit.Assert._
import io.github.mapogolions.cs.buffalo.{ Ops, Vals }

class Test1 {
  @Test def TestParseID: Unit = {
    assertEquals(Ops.parseID("hello"), Vals.ID("hello"))
    assertEquals(Ops.parseID("1hello"), Vals.ERROR)
    assertEquals(Ops.parseID("h2ello0"), Vals.ID("h2ello0"))
    assertEquals(Ops.parseID("h2ell$0"), Vals.ERROR)
  }

  @Test def TestParseInt: Unit = {
    assertEquals(Ops.parseInt("123", '+'), Vals.INT('+', 123))
    assertEquals(Ops.parseInt("34", '-'), Vals.INT('-', 34))
    assertEquals(Ops.parseInt("34i", '-'), Vals.ERROR)
    assertEquals(Ops.parseInt("03", '-'), Vals.INT('-', 3))
  }

  @Test def TestCheckChars: Unit = {
    assertTrue(Ops.checkChars(List('a', 'b', 'c'), 'b' :: 'c' :: Nil))
    assertFalse(Ops.checkChars(Nil, 'b' :: Nil))
    assertTrue(Ops.checkChars(List('a', 'b', 'c'), 'a' :: 'b' :: Nil))
    assertFalse(Ops.checkChars(List('a', 'b', 'c'), 'a' :: 'w' :: Nil))
  }

  @Test def TestCheckChar: Unit = {
    assertTrue(Ops.checkChar(List('a', 'b', 'c'), 'b'))
    assertFalse(Ops.checkChar(Nil, 'b'))
    assertFalse(Ops.checkChar(List('a', 'b', 'c'), 'w'))
  }

  @Test def TestImplode: Unit = {
    assertEquals(Ops.implode(List()), "")
    assertEquals(Ops.implode(List('a', 'b')), "ab")
  }

  @Test def TestExplode: Unit = {
    assertEquals(Ops.explode("ok!"), List('o', 'k', '!'))
  }

  @Test def TestParseBool(): Unit = {
    assertEquals(Ops.parseBool(":true:"), Vals.BOOL(true))
    assertEquals(Ops.parseBool(":false:"), Vals.BOOL(false))
    assertEquals(Ops.parseBool(":"), Vals.ERROR)
  }
}
