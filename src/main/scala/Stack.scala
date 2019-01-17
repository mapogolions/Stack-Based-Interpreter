package io.github.mapogolions.cs305.buffalo

import io.github.mapogolions.cs305.buffalo.Vals._
import io.github.mapogolions.cs305.buffalo.{ Env, Scope, Empty }

class Stack(val xs: List[Vals]) {
  def bind(env: Env) = xs match {
    case ERROR :: _ => (Stack(ERROR :: xs), env)
    case ID(name1) :: ID(name2) :: t => env.get(name1) match {
      case None => (Stack(ERROR :: xs), env) // attempt binding with unboud variable
      case Some(v) => (Stack(UNIT :: t), env.add(name2 -> v)) // shared reference
    }
    case v :: ID(k) :: t => (Stack(UNIT :: t), env.add(k, v)) // binding
    case _ => (Stack(ERROR :: xs), env)
  }

  def cond(env: Env) = xs match {
    case _ :: b :: BOOL(true) :: t => Stack(b :: t) -> env
    case a :: _ :: BOOL(false) :: t => Stack(a :: t) -> env
    case a :: b :: ID(name) :: t => env.get(name) match {
      case Some(BOOL(flag)) => (if (flag) Stack(b :: t) else Stack(a :: t)) -> env
      case _ => Stack(ERROR :: xs) -> env
    }
    case _ => Stack(ERROR :: xs) -> env
  }

  // unary
  def not(env: Env) = xs match {
    case BOOL(a) :: t => Stack(BOOL(!a) :: t) -> env
    case ID(name) :: t => env.get(name) match {
      case Some(BOOL(a)) => Stack(BOOL(!a) :: t) -> env
      case _ => Stack(ERROR :: xs) -> env
    }
    case _ => Stack(ERROR :: xs) -> env
  }

  def neg(env: Env) = xs match {
    case INT(a) :: t => Stack(INT(-a) :: t) -> env
    case ID(name) :: t => env.get(name) match {
      case Some(INT(a)) => Stack(INT(-a) :: t) -> env
      case _ => Stack(ERROR :: xs) -> env
    }
    case _ => Stack(ERROR :: xs) -> env
  }

  // binary 
  def swap(env: Env) = xs match {
    case a :: b :: t => Stack(b :: a :: t) -> env
    case _ => Stack(ERROR :: xs) -> env
  }

  def binary(f: ((Vals, Vals)) => Vals) = xs match {
    case a :: b :: t if (f(a -> b) != ERROR) => Stack(f(a -> b) :: t)
    case _ => Stack(ERROR :: xs)
  }

  def binaryBool(f: (Boolean, Boolean) => Vals, env: Env) = binary {
    (a, b) => (a, b) match {
      case (BOOL(v1), BOOL(v2)) => f(v1, v2)
      case (ID(name1), ID(name2)) =>
        (env.get(name1), env.get(name2)) match {
          case (Some(BOOL(v1)), Some(BOOL(v2))) => f(v1, v2)
          case _ => ERROR
        }
      case (ID(name), BOOL(v2)) =>
        env.get(name) match {
          case Some(BOOL(v1)) => f(v1, v2)
          case _ => ERROR
        }
      case (BOOL(v1), ID(name)) => 
        env.get(name) match {
          case Some(BOOL(v2)) => f(v1, v2)
          case _ => ERROR
        }
      case _ => ERROR
    }
  } -> env

  def or(env: Env) = binaryBool((a, b) => BOOL(a || b), env)
  def and(env: Env) = binaryBool((a, b) => BOOL(a && b), env)

  def binaryInt(f: (Int, Int) => Vals, env: Env) = binary {
    (a, b) => (a, b) match {
      case (INT(v1), INT(v2)) => f(v1, v2)
      case (ID(name1), ID(name2)) =>
        (env.get(name1), env.get(name2)) match {
          case (Some(INT(a)), Some(INT(b))) => f(a, b)
          case _ => ERROR
        }
      case (ID(name), INT(v2)) =>
        env.get(name) match {
          case Some(INT(v1)) => f(v1, v2)
          case _ => ERROR
        }
      case (INT(v1), ID(name)) => 
        env.get(name) match {
          case Some(INT(v2)) => f(v1, v2)
          case _ => ERROR
        }
      case _ => ERROR
    }
  } -> env

  def lessThan(env: Env) = binaryInt((a, b) => BOOL(b < a), env)
  def equality(env: Env) = binaryInt((a, b) => BOOL(a == b), env)
  def add(env: Env) = binaryInt((a, b) => INT(a + b), env)
  def sub(env: Env) = binaryInt((a, b) => INT(b - a), env)
  def mul(env: Env) = binaryInt((a, b) => INT(a * b), env)
  def div(env: Env) = binaryInt((a, b) => if (a != 0) INT(b / a) else ERROR, env)
  def rem(env: Env) = binaryInt((a, b) => if (a != 0) INT(b % a) else ERROR, env)

  def push(v: Vals, env: Env) = Stack(v :: xs) -> env
  def pop(env: Env) = xs match {
    case h :: t => Stack(t) -> env
    case _ => Stack(ERROR :: xs) -> env
  }

  override def equals(that: Any): Boolean = that match {
    case Stack(ys) if (ys == xs) => true
    case _ => false
  }
  override def toString = s"Stack($xs)"
}

object Stack {
  def apply(xs: List[Vals]) = new Stack(xs)
  def apply() = new Stack(Nil)
  def unapply(stk: Stack) = Some(stk.xs)
}
