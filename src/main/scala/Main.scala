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
      case IF :: t => {
        val (stack, ctx) = stacks.head.cond(env)
        exec(t, stack :: stacks.tail, ctx)
      }
      case LESSTHAN :: t => {
        val (stack, ctx) = stacks.head.lessThan(env)
        exec(t, stack :: stacks.tail, ctx)
      }
      case EQUAL :: t => {
        val (stack, ctx) = stacks.head.equality(env)
        exec(t, stack :: stacks.tail, ctx)
      }
      case NOT :: t => {
        val (stack, ctx) = stacks.head.not(env)
        exec(t, stack :: stacks.tail, ctx)
      }
      case OR :: t => {
        val (stack, ctx) = stacks.head.or(env)
        exec(t, stack :: stacks.tail, ctx)
      }
      case AND :: t => {
        val (stack, ctx) = stacks.head.and(env)
        exec(t, stack :: stacks.tail, ctx)
      }
      case SWAP :: t => {
        val (stack, ctx) = stacks.head.swap(env)
        exec(t, stack :: stacks.tail, ctx)
      }
      case NEG :: t => {
        val (stack, ctx) = stacks.head.neg(env)
        exec(t, stack :: stacks.tail, ctx)
      }
      case QUIT :: t => stacks -> env
      case _ => stacks -> env
    }
}
