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
          (PUSH(UNIT) :: t) -> env.add(funcName, CLOSURE(funcName, param, body.reverse, env))
        case h :: t => loop(h :: body, t)
      }

    loop(Nil, cmds)
  }

  def exec(cmds: List[Commands], stacks: List[Stack], env: Env): (List[Stack], Env) =
    cmds match {
      case PUSH(v) :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case frame :: frames => {
          val (stack, ctx) = frame.push(v, env)
          exec(t, stack :: frames, ctx)
        }
      }

      case POP :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case frame :: frames => {
          val (stack, ctx) = frame.pop(env)
          exec(t, stack :: frames, ctx)
        }
      }

      case FUN :: PUSH(ID(funcName)) :: PUSH(ID(param)) :: t => {
        val (cmds, ctx) = skip(t, funcName, param, env)
        exec(cmds, stacks, ctx)
      }

      case CALL :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case frame :: frames => {
          val (body, stack, ctx) = frame.callFunc(env)
          val (fstacks, fenv) = exec(body, Stack() :: Nil, ctx)
          val result = fstacks.head.xs.head
          val (newStack, _) = stack.push(result, env)
          exec(t, newStack :: frames, env)
        }
      }

      case RETURN :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case frame :: frames => frame match {
          case Stack(Nil) => (Stack(UNIT::Nil) :: Nil, env)
          case Stack(h :: t) => (Stack(h::Nil) :: Nil, env)
        }
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
        case frame :: frames => {
          val (stack, ctx) = frame.add(env)
          exec(t, stack :: frames, ctx)
        }
      }

      case SUB :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case frame :: frames => {
          val (stack, ctx) = frame.sub(env)
          exec(t, stack :: frames, ctx)
        }
      }

      case MUL :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case frame :: frames => {
          val (stack, ctx) = frame.mul(env)
          exec(t, stack :: frames, ctx)
        }
      }

      case DIV :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case frame :: frames => {
          val (stack, ctx) = frame.div(env)
          exec(t, stack :: frames, ctx)
        }
      }

      case REM :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case frame :: frames => {
          val (stack, ctx) = frame.rem(env)
          exec(t, stack :: frames, ctx)
        }
      }

      case BIND :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case frame :: frames => {
          val (stack, ctx) = frame.bind(env)
          exec(t, stack :: frames, ctx)
        }
      }

      case IF :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case frame :: frames => {
          val (stack, ctx) = frame.cond(env)
          exec(t, stack :: frames, ctx)
        }
      }

      case LESSTHAN :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case frame :: frames => {
          val (stack, ctx) = frame.lessThan(env)
          exec(t, stack :: frames, ctx)
        }
      }

      case EQUAL :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case frame :: frames => {
          val (stack, ctx) = frame.equality(env)
          exec(t, stack :: frames, ctx)
        }
      }

      case NOT :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case frame :: frames => {
          val (stack, ctx) = frame.not(env)
          exec(t, stack :: frames, ctx)
        }
      }

      case OR :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case frame :: frames => {
          val (stack, ctx) = frame.or(env)
          exec(t, stack :: frames, ctx)
        }
      }

      case AND :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case frame :: frames => {
          val (stack, ctx) = frame.and(env)
          exec(t, stack :: frames, ctx)
        }
      }

      case SWAP :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case frame :: frames => {
          val (stack, ctx) = frame.swap(env)
          exec(t, stack :: frames, ctx)
        }
      }

      case NEG :: t => stacks match {
        case Nil => exec(t, Stack(ERROR :: Nil) :: stacks, env)
        case frame :: frames => {
          val (stack, ctx) = frame.neg(env)
          exec(t, stack :: frames, ctx)
        }
      }

      case QUIT :: t => stacks -> env
      case _ => stacks -> env
    }
}
