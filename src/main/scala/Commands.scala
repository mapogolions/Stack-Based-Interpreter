package io.github.mapogolions.cs305.buffalo

import io.github.mapogolions.cs305.buffalo.Vals


enum Commands {
  case REM
  case PUSH(val content: Vals)
  case POP
  case DIV
  case MUL
  case SUB
  case ADD
  case BIND
  case QUIT
}
