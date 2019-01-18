package io.github.mapogolions.cs305.buffalo

import io.github.mapogolions.cs305.buffalo.Vals._
import io.github.mapogolions.cs305.buffalo.{ Env, Scope, Empty }
import io.github.mapogolions.cs305.buffalo.Commands._
import io.github.mapogolions.cs305.buffalo.Stack


object Main {
  // rollup function body
  def skip(cmds: List[Commands], funcName: String, param: String, env: Env) = {
    @annotation.tailrec
    def loop(body: List[Commands], rest: List[Commands]): (List[Commands], Env) =
      rest match {
        case Nil => rest -> env
        case FUNEND :: t =>
          t -> env.add(funcName, CLOSURE(funcName, param, body.reverse, env))
        case h :: t => loop(h :: body, t)
      }

    loop(Nil, cmds)
  }

  def exec(cmds: List[Commands], stacks: List[Stack], env: Env): (List[Stack], Env) =
    cmds match {
      case PUSH(v) :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case stack :: rest => {
          val (stk, ctx) = stack.push(v, env)
          exec(t, stk :: rest, ctx)
        }
      }

      case POP :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case stack :: rest => {
          val (stk, ctx) = stack.pop(env)
          exec(t, stk :: rest, ctx)
        }
      }

      case FUN :: PUSH(ID(funcName)) :: PUSH(ID(param)) :: t => {
        val (cmds, ctx) = skip(t, funcName, param, env)
        exec(cmds, stacks, ctx)
      }

      case CALL :: t => {
        val (fcmds, fstack, fenv) = stacks.head.callFunc(env)
        val (fstacks, ctx) = exec(fcmds, fstack :: Nil, fenv)
        exec(t, fstacks.head :: stacks.tail, ctx)
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

      case ADD :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case stack :: rest => {
          val (stk, ctx) = stack.add(env)
          exec(t, stk :: rest, ctx)
        }
      }

      case SUB :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case stack :: rest => {
          val (stk, ctx) = stack.sub(env)
          exec(t, stk :: rest, ctx)
        }
      }

      case MUL :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case stack :: rest => {
          val (stk, ctx) = stack.mul(env)
          exec(t, stk :: rest, ctx)
        }
      }

      case DIV :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case stack :: rest => {
          val (stk, ctx) = stack.div(env)
          exec(t, stk :: rest, ctx)
        }
      }

      case REM :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case stack :: rest => {
          val (stk, ctx) = stack.rem(env)
          exec(t, stk :: rest, ctx)
        }
      }

      case BIND :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case stack :: rest => {
          val (stk, ctx) = stack.bind(env)
          exec(t, stk :: rest, ctx)
        }
      }

      case IF :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case stack :: rest => {
          val (stk, ctx) = stack.cond(env)
          exec(t, stk :: rest, ctx)
        }
      }

      case LESSTHAN :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case stack :: rest => {
          val (stk, ctx) = stack.lessThan(env)
          exec(t, stk :: rest, ctx)
        }
      }

      case EQUAL :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case stack :: rest => {
          val (stk, ctx) = stack.equality(env)
          exec(t, stk :: rest, ctx)
        }
      }

      case NOT :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case stack :: rest => {
          val (stk, ctx) = stack.not(env)
          exec(t, stk :: rest, ctx)
        }
      }

      case OR :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case stack :: rest => {
          val (stk, ctx) = stack.or(env)
          exec(t, stk :: rest, ctx)
        }
      }

      case AND :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case stack :: rest => {
          val (stk, ctx) = stack.and(env)
          exec(t, stk :: rest, ctx)
        }
      }

      case SWAP :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case stack :: rest => {
          val (stk, ctx) = stack.swap(env)
          exec(t, stk :: rest, ctx)
        }
      }

      case NEG :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case stack :: rest => {
          val (stk, ctx) = stack.neg(env)
          exec(t, stk :: rest, ctx)
        }
      }

      case QUIT :: t => stacks -> env
      case _ => stacks -> env
    }
}
