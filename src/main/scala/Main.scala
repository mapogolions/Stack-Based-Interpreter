package io.github.mapogolions.cs305.buffalo

import io.github.mapogolions.cs305.buffalo.Vals._
import io.github.mapogolions.cs305.buffalo.{ Env, Scope, Empty }
import io.github.mapogolions.cs305.buffalo.Commands._
import io.github.mapogolions.cs305.buffalo.Stack


object Main {
  // Interpreter
  def exec(cmds: List[Commands], stack: Stack, env: Env): (Stack, Env) =
    cmds match {
      case PUSH(v) :: t => {
        val (stk, ctx) = stack.push(v, env)
        exec(t, stk, ctx)
      }
      case POP :: t => {
        val (stk, ctx) = stack.pop(env)
        exec(t, stk, ctx)
      }
      case ADD :: t => {
        val (stk, ctx) = stack.add(env)
        exec(t, stk, ctx)
      }
      case SUB :: t => {
        val (stk, ctx) = stack.sub(env)
        exec(t, stk, ctx)
      }
      case MUL :: t => {
        val (stk, ctx) = stack.mul(env)
        exec(t, stk, ctx)
      }
      case DIV :: t => {
        val (stk, ctx) = stack.div(env)
        exec(t, stk, ctx)
      }
      case REM :: t => {
        val (stk, ctx) = stack.rem(env)
        exec(t, stk, ctx)
      }
      case BIND ::t => {
        val (stk, ctx) = stack.bind(env)
        exec(t, stk, ctx)
      }
      case _ => stack -> env
    }
}
