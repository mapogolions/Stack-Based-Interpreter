package io.github.mapogolions.cs.buffalo


object Main {
  def main(args: Array[String]): Unit = {
    msg
  }

  def msg = "I was compiled by dotty :)"

}

enum Vals {
  case INT(val sign: Char, val content: Int)
  case BOOL(val content: Boolean)
  case STRING(val content: String)
  case ID(val content: String)
  case ERROR
}



/* object Parser {
  type Stack = List[Vals]
  import Vals._

  def eval(xs: List[String], stk: Stack): Stack = xs match {
    case "push" :: t => ???
    case "pop" :: t => eval(t, Stack.pop(stk))
    case "add" :: t => eval(t, Stack.add(stk))
    case _ => sys.error("Invalid command")
  }
}

object Stack {
  import Vals._
  type Stack = List[Vals]

  // TODO: Need implement support for bindings Names
  def add(xs: Stack) = xs match {
    case INT(a) :: INT(b) :: t => INT(a + b) :: t
    case _ => Vals.ERROR :: xs
  }

  def pop(xs: Stack) = xs match {
    case Nil => Vals.ERROR :: xs
    case _ :: t => t
  }
  def empty = Nil
} */
