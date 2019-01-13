import org.junit.Test
import org.junit.Assert._
import io.github.mapogolions.cs305.buffalo.{ Ops, Vals, Commands }

class Test1 {
  import Vals._
  import Commands._

  @Test def TestParseCommands: Unit = {
    assertEquals(Ops.parseCommands(Nil), Nil)

    assertEquals(
      Ops.parseCommands(List("push 20.0")),
      PUSH(ERROR) :: Nil
    );

    assertEquals(
      Ops.parseCommands(
        List(
          "push 10",
          "push a",
          "push name1",
          "push -10"
        )
      ),
      List(
        PUSH(INT('+', 10)),
        PUSH(ID("a")),
        PUSH(ID("name1")),
        PUSH(INT('-', 10))
      )
    );

    assertEquals(
      Ops.parseCommands(
        List(
          "add",
          "mul",
          "quit",
          "div",
          "sub"
        )
      ),
      List(
        ADD,
        MUL,
        QUIT,
        DIV,
        SUB
      )
    );

    assertEquals(
      Ops.parseCommands(
        List(
          "add",
          ":false:",
          "mul",
          ":error:",
          ":true:"
        )
      ),
      List(
        ADD,
        PUSH(BOOL(false)),
        MUL,
        PUSH(ERROR),
        PUSH(BOOL(true))
      )
    );
  }

  @Test def TestParseLiteral: Unit = {
    assertEquals(Ops.parseLiteral("-30"), Vals.INT('-', 30))
    assertEquals(Ops.parseLiteral("-30l"), Vals.ERROR)
    assertEquals(Ops.parseLiteral(":error:"), Vals.ERROR)
    assertEquals(Ops.parseLiteral(":false:"), Vals.BOOL(false))
    assertEquals(Ops.parseLiteral(":true:"), Vals.BOOL(true))
    assertEquals(Ops.parseLiteral(""), Vals.ERROR)
    assertEquals(Ops.parseLiteral("a"), Vals.ID("a"))
    assertEquals(Ops.parseLiteral("name1"), Vals.ID("name1"))
    assertEquals(Ops.parseLiteral("1name"), Vals.ERROR)
    assertEquals(Ops.parseLiteral("user_name"), Vals.ERROR)
    assertEquals(Ops.parseLiteral("$some"), Vals.ERROR)
    assertEquals(Ops.parseLiteral("30"), Vals.INT('+', 30))
    assertEquals(Ops.parseLiteral("30.2"), Vals.ERROR)
    assertEquals(Ops.parseLiteral("-30.2"), Vals.ERROR)
  }

  @Test def TestStripNewline: Unit = {
    assertEquals(Ops.stripNewline("\ntx\nt\n"), "txt")
  }

  @Test def TestStripQuotes: Unit = {
    assertEquals(Ops.stripQuotes("\"...\""), "...")
    assertEquals(Ops.stripQuotes("\"..."), "...")
    assertEquals(Ops.stripQuotes("...\""), "...")
    assertEquals(Ops.stripQuotes("..\"."), "..\".")
    assertEquals(Ops.stripQuotes("\"\""), "")
    assertEquals(Ops.stripQuotes("\""), "")
    assertEquals(Ops.stripQuotes("..."), "...")
    assertEquals(Ops.stripQuotes(""), "")
  }

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
