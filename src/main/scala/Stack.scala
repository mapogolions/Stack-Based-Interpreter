package io.github.mapogolions.cs305.buffalo

import io.github.mapogolions.cs305.buffalo.Vals._


class Stack(val xs: List[Vals]) {
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
