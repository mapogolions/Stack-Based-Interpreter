import org.junit.Test
import org.junit.Assert._
import io.github.mapogolions.cs305.buffalo.Vals._
import io.github.mapogolions.cs305.buffalo.Commands._
import io.github.mapogolions.cs305.buffalo.{ Env, Scope, Empty }


class TestEnv {
  @Test def TestGetEnv: Unit = {
    val env = Scope(
      Map("a" -> STR("hello")),
      Scope(
        Map("b" -> BOOL(true)),
        Empty
      )
    )
    assertEquals(env.get("a"), Some(STR("hello")))
    assertEquals(env.get("b"), Some(BOOL(true)))

    // Shadowing b - binding
    val env1 = env.add("b" -> BOOL(false))
    assertEquals(env1.get("b"), Some(BOOL(false)))
  }

  @Test def TestAddEnv: Unit = {
    val env0 = Scope(
      Map("a" -> STR("hello world")),
      Empty
    )
    val env1 = env0.add("b" -> BOOL(true))

    assertTrue(env0.has("a"))
    assertFalse(env0.has("b"))

    assertTrue(env1.has("a"))
    assertTrue(env1.has("b"))
  }

  @Test def TestHasEnv: Unit = {
    val env = Scope(
      Map("a" -> INT(10)),
      Scope(
        Map("b" -> INT(2)),
        Scope(
          Map("res" -> UNIT),
          Empty
        )
      )
    )
    assertTrue(env.has("a"))
    assertTrue(env.has("b"))
    assertFalse(env.has("c"))
    assertTrue(env.has("res"))
  }
}
