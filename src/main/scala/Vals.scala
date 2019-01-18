package io.github.mapogolions.cs305.buffalo

import io.github.mapogolions.cs305.buffalo.Env
import io.github.mapogolions.cs305.buffalo.Commands


enum Vals { self =>
  override def toString = self match {
    case ERROR => "ERROR"
    case UNIT  => "UNIT"
    case INT(v) => s"INT($v)"
    case BOOL(v) => s"BOOL($v)"
    case STR(v) => s"STR($v)"
    case ID(name) => s"ID($name)"
    case CLOSURE(name, args, _, body) => s"FUN($name)<${body}>"
  }

  case ERROR
  case UNIT
  case INT(val content: Int)
  case BOOL(val content: Boolean)
  case STR(val content: String)
  case ID(val content: String)
  case CLOSURE(val name: String, arg: String, body: List[Commands], ctx: Env)
}

// PUSH(ID("a"))
// PUSH(ID("b"))
// FUN
// PUSH(
//   CLOJURE(
//     ID("identifier"), function name
//       ID("a") :: ID("b") :: Nil, arguments,
//     CTX: env, окружение в котором была создана функция
//     BODY(PUSH(INT(10) :: PUSH(INT10) :: ADD :: RETURN))
//   )
// )
// FUNEND - аналог для BIND

// Вызов функции выглядит следующим образом:
//   -- commands --                           -- stack --
// PUSH(ID("increment"))                   INT(10)
// PUSH(INT(10))                           ID("increment")
// CALL        ->                          return function value
//

// FUN .. FUNEND от LET .. END - отличает механизмом отложенного вычисления
