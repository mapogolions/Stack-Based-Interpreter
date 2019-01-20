package io.github.mapogolions.cs305.buffalo

import io.github.mapogolions.cs305.buffalo.Vals._
import io.github.mapogolions.cs305.buffalo.{ Env, Scope, Empty }
import io.github.mapogolions.cs305.buffalo.Commands._
import io.github.mapogolions.cs305.buffalo.Stack
import io.github.mapogolions.cs305.buffalo.Parse
import scala.io.Source
import java.io._


object Main {
  def channel(source: String, target: String) = {
    val lines = Source.fromFile(source).getLines.toList
    val (stack, env) = exec(Parse.commands(lines), Stack(), Empty)
    val pw = new PrintWriter(new File(target))
    stack.xs.foreach(line => pw.write(line.toString))
    pw.close
  }

  def exec(cmds: List[Commands], stack: Stack=Stack(), env: Env=Empty): (Stack, Env) =
    cmds match {
      case PUSH(v) :: t => {
        val (newStack, ctx) = stack.push(v, env)
        exec(t, newStack, ctx)
      }

      case POP:: t => {
        val (newStack, ctx) = stack.pop(env)
        exec(t, newStack, ctx)
      }

      case RETURN :: t => stack -> env

      case CALL :: t => {
        val (funBody, newStack, statEnv) = stack.callFunc(env)
        funBody match {
          case Nil => exec(t, newStack, env)
          case _ => (exec(funBody, Stack(), statEnv), newStack) match {
            case ((Stack(Nil), _), Stack(ys)) => exec(t, Stack(UNIT :: ys), env)
            case ((Stack(ID(name):: _), ctx), Stack(ys)) => ctx.get(name) match {
              case Some(v) => exec(t, Stack(v :: ys), env)
              case _ => exec(t, Stack(ERROR :: ys), env)
            }
            case ((Stack(h :: _), _), Stack(ys)) => exec(t, Stack(h :: ys), env)
          }
        }
      }

      case FUN :: PUSH(ID(f)) :: PUSH(ID(arg)) :: t => {
        val (funBody, restCmds) = funEnd(FUN :: t)
        exec(restCmds, Stack(UNIT :: stack.xs), env.add(f -> CLOSURE(f, arg, funBody, env)))
      }

      case LET :: t => {
        val (letCmds, restCmds) = letEnd(LET :: t)
        val (newStack, ctx) = exec(letCmds, Stack(), Scope(Map(), env))
        (newStack, stack) match {
          case (Stack(Nil), Stack(ys)) => exec(restCmds, Stack(UNIT :: ys), env)
          case (Stack(h :: t), Stack(ys)) => exec(restCmds, Stack(h :: ys), env)
        }
      }

      case ADD :: t => {
        val (newStack, ctx) = stack.add(env)
        exec(t, newStack, ctx)
      }

      case SUB :: t => {
        val (newStack, ctx) = stack.sub(env)
        exec(t, newStack, ctx)
      }

      case MUL :: t => {
        val (newStack, ctx) = stack.mul(env)
        exec(t, newStack, ctx)
      }

      case DIV :: t => {
        val (newStack, ctx) = stack.div(env)
        exec(t, newStack, ctx)
      }

      case REM :: t => {
        val (newStack, ctx) = stack.rem(env)
        exec(t, newStack, ctx)
      }

      case BIND :: t => {
        val (newStack, ctx) = stack.bind(env)
        exec(t, newStack, ctx)
      }

      case IF :: t => {
        val (newStack, ctx) = stack.cond(env)
        exec(t, newStack, ctx)
      }

      case LESSTHAN :: t => {
        val (newStack, ctx) = stack.lessThan(env)
        exec(t, newStack, ctx)
      }

      case EQUAL :: t => {
        val (newStack, ctx) = stack.equality(env)
        exec(t, newStack, ctx)
      }

      case NOT :: t => {
        val (newStack, ctx) = stack.not(env)
        exec(t, newStack, ctx)
      }

      case OR :: t => {
        val (newStack, ctx) = stack.or(env)
        exec(t, newStack, ctx)
      }

      case AND :: t => {
        val (newStack, ctx) = stack.and(env)
        exec(t, newStack, ctx)
      }

      case SWAP :: t => {
        val (newStack, ctx) = stack.swap(env)
        exec(t, newStack, ctx)
      }

      case NEG :: t => {
        val (newStack, ctx) = stack.neg(env)
        exec(t, newStack, ctx)
      }

      case QUIT :: t => stack -> env
      case _ => stack -> env
    }

    def letEnd(cmds: List[Commands]) = {
    def loop(
      acc: List[Commands],
      xs: List[Commands],
      balanced: List[Commands]): (List[Commands], List[Commands]) =
      xs match {
        case LET :: t if (balanced != Nil) => loop(LET :: acc, t, LET :: balanced)
        case LET :: t => loop(acc, t, LET :: balanced)
        case END :: t if (letEndBalanced((END :: balanced).reverse)) => acc.reverse -> t
        case END :: t => loop(END :: acc, t, END :: balanced)
        case cmd :: t => loop(cmd :: acc, t, balanced)
        case _ => acc.reverse -> xs
      }
    loop(Nil, cmds, Nil)
  }

  def funEnd(cmds: List[Commands]) = {
    def loop(
      acc: List[Commands],
      xs: List[Commands],
      balanced: List[Commands]): (List[Commands], List[Commands]) =
      xs match {
        case FUN :: t if (balanced != Nil) => loop(FUN :: acc, t, FUN :: balanced)
        case FUN :: t => loop(acc, t, FUN :: balanced)
        case FUNEND :: t if (funEndBalanced((FUNEND :: balanced).reverse)) => acc.reverse -> t
        case FUNEND :: t => loop(FUNEND :: acc, t, FUNEND :: balanced)
        case cmd :: t => loop(cmd :: acc, t, balanced)
        case _ => acc.reverse -> xs
      }
    loop(Nil, cmds, Nil)
  }

  def letEndBalanced(tokens: List[Commands]): Boolean = {
    @annotation.tailrec
    def loop(tokens: List[Commands], stack: List[Commands]): Boolean = {
      (tokens, stack) match {
        case (LET :: t, stack) => loop(t, LET :: stack)
        case (END :: t, LET :: t2) => loop(t, t2)
        case (Nil, Nil) => true
        case _ => false
      }
    }
    loop(tokens, Nil)
  }

  def funEndBalanced(tokens: List[Commands]): Boolean = {
    @annotation.tailrec
    def loop(tokens: List[Commands], stack: List[Commands]): Boolean = {
      (tokens, stack) match {
        case (FUN :: t, stack) => loop(t, FUN :: stack)
        case (FUNEND :: t, FUN :: t2) => loop(t, t2)
        case (Nil, Nil) => true
        case _ => false
      }
    }
    loop(tokens, Nil)
  }
}
