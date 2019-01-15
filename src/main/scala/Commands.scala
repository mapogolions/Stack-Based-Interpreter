package io.github.mapogolions.cs305.buffalo

import io.github.mapogolions.cs305.buffalo.Vals


enum Commands {
  case BIND
  case IF
  case LESSTHAN
  case EQUAL
  case NOT
  case OR
  case AND
  case SWAP
  case NEG
  case REM
  case PUSH(val content: Vals)
  case POP
  case DIV
  case MUL
  case SUB
  case ADD
  case QUIT
}
