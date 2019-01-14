package io.github.mapogolions.cs305.buffalo


enum Vals {
  case ERROR
  case UNIT;
  case INT(val content: Int)
  case BOOL(val content: Boolean)
  case STR(val content: String)
  case ID(val content: String)
}
