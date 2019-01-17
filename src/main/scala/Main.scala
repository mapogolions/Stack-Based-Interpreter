package io.github.mapogolions.cs305.buffalo

import io.github.mapogolions.cs305.buffalo.Vals._
import io.github.mapogolions.cs305.buffalo.{ Env, Scope, Empty }
import io.github.mapogolions.cs305.buffalo.Commands._
import io.github.mapogolions.cs305.buffalo.Stack


object Main {
  // Interpreter
  def exec(cmds: List[Commands], stacks: List[Stack], env: Env): (List[Stack], Env) =
    cmds match {
      case PUSH(v) :: t => {
        val (stack, ctx) = stacks.head.push(v, env)
        exec(t, stack :: stacks.tail, ctx)
      }
      case POP :: t => {
        val (stack, ctx) = stacks.head.pop(env)
        exec(t, stack :: stacks.tail, ctx)
      }
      case LET :: t => exec(t, Stack() :: stacks, Scope(Map(), env))
      case END :: t => {
        stacks match {
          case Stack(Nil) :: Stack(ys) :: rest => exec(t, Stack(UNIT :: ys) :: rest, env.parent)
          case Stack(ID(name) :: _) :: Stack(ys) :: rest => env.get(name) match {
            case Some(v) => exec(t, Stack(v :: ys) :: rest, env.parent)
            case _ => exec(t, Stack(ERROR :: ys) :: rest, env.parent)
          }
          case Stack(v :: _) :: Stack(ys) :: rest => exec(t, Stack(v :: ys) :: rest, env.parent)
          case _ => stacks.head match {
              case Stack(ys) => exec(t, Stack(ERROR :: ys) :: stacks.tail, env)
            }
        }
      }
      case ADD :: t => {
        val (stack, ctx) = stacks.head.add(env)
        exec(t, stack :: stacks.tail, ctx)
      }
      case SUB :: t => {
        val (stack, ctx) = stacks.head.sub(env)
        exec(t, stack :: stacks.tail, ctx)
      }
      case MUL :: t => {
        val (stack, ctx) = stacks.head.mul(env)
        exec(t, stack :: stacks.tail, ctx)
      }
      case DIV :: t => {
        val (stack, ctx) = stacks.head.div(env)
        exec(t, stack :: stacks.tail, ctx)
      }
      case REM :: t => {
        val (stack, ctx) = stacks.head.rem(env)
        exec(t, stack :: stacks.tail, ctx)
      }
      case BIND ::t => {
        val (stack, ctx) = stacks.head.bind(env)
        exec(t, stack :: stacks.tail, ctx)
      }
      case _ => stacks -> env
    }
}
