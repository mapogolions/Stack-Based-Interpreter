package io.github.mapogolions.cs305.buffalo


object Main {
  def main(args: Array[String]): Unit = {
    msg
  }

  def msg = "I was compiled by dotty :)"

}


/* 
type ('k 'v) env= {
  parent : null | ('k 'v) env ;
  bindings : ('k 'v) map
}
 */

trait Env
case object Blank extends Env
case class Scope(val bindings: Map[String, Vals]) extends Env

enum Commands {
  case PUSH(val content: Vals)
  case POP
  case DIV
  case MUL
  case SUB
  case ADD
  case BIND
  case QUIT
}

enum Vals {
  case ERROR
  case UNIT;
  case INT(val sign: Char, val content: Int) {
    def hello = "stopper"
  }
  case BOOL(val content: Boolean)
  case STR(val content: String)
  case ID(val content: String)
}

