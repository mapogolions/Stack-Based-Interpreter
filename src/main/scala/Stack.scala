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

  def cond = xs match {
    case _ :: b :: BOOL(true) :: t => Stack(b :: t)
    case a :: _ :: BOOL(false) :: t => Stack(a :: t)
    case _ => Stack(ERROR :: xs)
  }

  def lessThan = xs match {
    case INT(a) :: INT(b) :: t => Stack(BOOL(b < a) :: t)
    case _ => Stack(ERROR :: xs)
  }

  def equality = xs match {
    case INT(a) :: INT(b) :: t => Stack(BOOL(a == b) :: t)
    case _ => Stack(ERROR :: xs)
  }

  def not = xs match {
    case BOOL(a) :: t => Stack(BOOL(!a) :: t)
    case _ => Stack(ERROR :: xs)
  }

  def or = xs match {
    case BOOL(a) :: BOOL(b) :: t => Stack(BOOL(a || b) :: t)
    case _ => Stack(ERROR :: xs)
  }

  def and = xs match {
    case BOOL(a) :: BOOL(b) :: t => Stack(BOOL(a && b) :: t)
    case _ => Stack(ERROR :: xs)
  }

  def swap = xs match {
    case a :: b :: t => Stack(b :: a :: t)
    case _ => Stack(ERROR :: xs)
  }

  def neg = xs match {
    case INT(a) :: t => Stack(INT(-a) :: t)
    case _ => Stack(ERROR :: xs)
  }

  // TODO: Add support for bind!
  def add = xs match {
    case INT(a) :: INT(b) :: t => Stack(INT(a + b) :: t)
    case _ => Stack(ERROR :: xs)
  }

  // TODO: Add support for bind!
  def sub = xs match {
    case INT(a) :: INT(b) :: t => Stack(INT(b - a) :: t)
    case _ => Stack(ERROR :: xs)
  }

  // TODO: Add support for bind!
  def mul = xs match {
    case INT(a) :: INT(b) :: t => Stack(INT(a * b) :: t)
    case _ => Stack(ERROR :: xs)
  }

  // TODO: Add support for bind!
  def div = xs match {
    case INT(a) :: INT(b) :: t if (a != 0) => Stack(INT(b / a) :: t)
    case _ => Stack(ERROR :: xs)
  }

  // TODO: Add support for bind!
  def rem = xs match {
    case INT(a) :: INT(b) :: t if (a != 0) => Stack(INT(b % a) :: t)
    case _ => Stack(ERROR :: xs)
  }

  def push(v: Vals) = Stack(v :: xs)
  def pop = xs match {
    case h :: t => Stack(t)
    case _ => Stack(ERROR :: xs)
  }

  override def equals(that: Any): Boolean = that match {
    case Stack(ys) if (ys == xs) => true
    case _ => false
  }
}

object Stack {
  def apply(xs: List[Vals]) = new Stack(xs)
  def apply() = new Stack(Nil)
  def unapply(stk: Stack) = Some(stk.xs)
}
