package io.github.mapogolions.cs305.buffalo


enum Vals { self =>
  override def toString = self match {
    case ERROR => "ERROR"
    case UNIT  => "UNIT"
    case INT(v) => s"INT($v)"
    case BOOL(v) => s"BOOL($v)"
    case STR(v) => s"STR($v)"
    case ID(name) => s"ID($name)"
  }
  
  case ERROR
  case UNIT
  case INT(val content: Int)
  case BOOL(val content: Boolean)
  case STR(val content: String)
  case ID(val content: String)
}
