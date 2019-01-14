import org.junit.Test
import org.junit.Assert._
import io.github.mapogolions.cs305.buffalo.Vals._
import io.github.mapogolions.cs305.buffalo.Commands._
import io.github.mapogolions.cs305.buffalo.Parse


class Test1 {
  @Test def TestParseCommands: Unit = {
    assertEquals(Parse.commands(Nil), Nil)

    assertEquals(
      Parse.commands(List("push 20.0")),
      PUSH(ERROR) :: Nil
    );

    assertEquals(
      Parse.commands(
        List(
          "push 10",
          "push a",
          "push name1",
          "push -10"
        )
      ),
      List(
        PUSH(INT(10)),
        PUSH(ID("a")),
        PUSH(ID("name1")),
        PUSH(INT(-10))
      )
    );

    assertEquals(
      Parse.commands(
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
      Parse.commands(
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
    assertEquals(Parse.literal("-30"), INT(-30))
    assertEquals(Parse.literal("-30l"), ERROR)
    assertEquals(Parse.literal(":error:"), ERROR)
    assertEquals(Parse.literal(":false:"), BOOL(false))
    assertEquals(Parse.literal(":true:"), BOOL(true))
    assertEquals(Parse.literal(""), ERROR)
    assertEquals(Parse.literal("a"), ID("a"))
    assertEquals(Parse.literal("name1"), ID("name1"))
    assertEquals(Parse.literal("1name"), ERROR)
    assertEquals(Parse.literal("user_name"), ERROR)
    assertEquals(Parse.literal("$some"), ERROR)
    assertEquals(Parse.literal("30"), INT(30))
    assertEquals(Parse.literal("30.2"), ERROR)
    assertEquals(Parse.literal("-30.2"), ERROR)
  }

  @Test def TestStripNewline: Unit = {
    assertEquals(Parse.stripNewline("\ntx\nt\n"), "txt")
  }

  @Test def TestStripQuotes: Unit = {
    assertEquals(Parse.stripQuotes("\"...\""), "...")
    assertEquals(Parse.stripQuotes("\"..."), "...")
    assertEquals(Parse.stripQuotes("...\""), "...")
    assertEquals(Parse.stripQuotes("..\"."), "..\".")
    assertEquals(Parse.stripQuotes("\"\""), "")
    assertEquals(Parse.stripQuotes("\""), "")
    assertEquals(Parse.stripQuotes("..."), "...")
    assertEquals(Parse.stripQuotes(""), "")
  }

  @Test def TestParseID: Unit = {
    assertEquals(Parse.identifier("hello"), ID("hello"))
    assertEquals(Parse.identifier("1hello"), ERROR)
    assertEquals(Parse.identifier("h2ello0"), ID("h2ello0"))
    assertEquals(Parse.identifier("h2ell$0"), ERROR)
  }

  @Test def TestParseInt: Unit = {
    assertEquals(Parse.integer("123", 1), INT(123))
    assertEquals(Parse.integer("34", -1), INT(-34))
    assertEquals(Parse.integer("34i", -1), ERROR)
    assertEquals(Parse.integer("03",  -1), INT(-3))
  }

  @Test def TestCheckChars: Unit = {
    assertTrue(Parse.checkChars(List('a', 'b', 'c'), 'b' :: 'c' :: Nil))
    assertFalse(Parse.checkChars(Nil, 'b' :: Nil))
    assertTrue(Parse.checkChars(List('a', 'b', 'c'), 'a' :: 'b' :: Nil))
    assertFalse(Parse.checkChars(List('a', 'b', 'c'), 'a' :: 'w' :: Nil))
  }

  @Test def TestCheckChar: Unit = {
    assertTrue(Parse.checkChar(List('a', 'b', 'c'), 'b'))
    assertFalse(Parse.checkChar(Nil, 'b'))
    assertFalse(Parse.checkChar(List('a', 'b', 'c'), 'w'))
  }

  @Test def TestImplode: Unit = {
    assertEquals(Parse.implode(List()), "")
    assertEquals(Parse.implode(List('a', 'b')), "ab")
  }

  @Test def TestExplode: Unit = {
    assertEquals(Parse.explode("ok!"), List('o', 'k', '!'))
  }

  @Test def TestParseBool(): Unit = {
    assertEquals(Parse.boolean(":true:"), BOOL(true))
    assertEquals(Parse.boolean(":false:"), BOOL(false))
    assertEquals(Parse.boolean(":"), ERROR)
  }
}
