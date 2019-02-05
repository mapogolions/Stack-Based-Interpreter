package io.github.mapogolions.cs305.buffalo

import io.github.mapogolions.cs305.buffalo.Env
import io.github.mapogolions.cs305.buffalo.Commands


enum Vals { self =>
  override def toString = self match {
    case ERROR => ":error:"
    case UNIT  => ":unit:"
    case INT(v) => s"$v"
    case BOOL(v) => s"$v"
    case STR(v) => v
    case ID(name) => name
    case CLOSURE(name, args, _, body) => s"<fun>($name)<${body}>"
  }

  case ERROR
  case UNIT
  case INT(val content: Int)
  case BOOL(val content: Boolean)
  case STR(val content: String)
  case ID(val content: String)
  case CLOSURE(
    val name: String, 
    val arg: String, 
    val body: List[Commands], 
    val ctx: Env
  )
}