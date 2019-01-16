package io.github.mapogolions.cs305.buffalo

import io.github.mapogolions.cs305.buffalo.Vals._
import io.github.mapogolions.cs305.buffalo.Commands._


object Parse {
  // Parse strings into commands list
  def commands(xs: List[String]): List[Commands] = xs match {
    case Nil => Nil
    case line :: ls => line.split(' ').toList match {
      case "push" :: t    => PUSH(literal(t.mkString(""))) :: commands(ls)
      case "pop" :: t     => POP :: commands(ls)
      case "add" :: t     => ADD :: commands(ls)
      case "sub" :: t     => SUB :: commands(ls)
      case "mul" :: t     => MUL :: commands(ls)
      case "div" :: t     => DIV :: commands(ls)
      case "bind" :: t    => BIND :: commands(ls)
      case "let" :: t     => LET :: commands(ls)
      case "end" :: t     => END :: commands(ls)
      case ":true:" :: t  => PUSH(BOOL(true)) :: commands(ls)
      case ":false:" :: t => PUSH(BOOL(false)) :: commands(ls)
      case ":error:" :: t => PUSH(ERROR) :: commands(ls)
      case "quit" :: t    => QUIT :: commands(ls)
      case _              => Nil
    }
  }

  def literal(src: String): Vals =
    explode(src) match {
      case '-' :: t => integer(implode(t), -1)
      case '"' :: t => if (checkASCII(t)) STR(stripQuotes(src)) else ERROR
      case ':' :: t => boolean(src)
      case h :: t => if (checkChar(digits, h)) integer(src, 1) else identifier(src)
      case _ => ERROR
    }

  def identifier(src: String): Vals =
    explode(src) match {
      case h :: t if (checkChar(alphabets, h) && checkChars(alphanums, t)) => ID(src)
      case _ => ERROR
    }

  def integer(src: String, sign: Int): Vals =
    if (checkChars(digits, src.toList)) INT(sign * src.toInt)
    else ERROR

  def boolean(src: String): Vals = src.trim match {
    case ":true:" => BOOL(true)
    case ":false:" => BOOL(false)
    case _ => ERROR
  }

  def stripNewline(src: String): String = {
    def loop(xs: List[Char]): List[Char] = xs match {
      case Nil => Nil
      case '\n' :: t => loop(t)
      case h :: t => h :: loop(t)
    }

    implode(loop(explode(src)))
  }

  def stripQuotes(src: String): String = {
    def stripHead(xs: List[Char]) = xs match {
      case '"' :: t => t
      case _ => xs
    }
    def stripTail(xs: List[Char]): List[Char] = xs match {
      case Nil => Nil
      case '"' :: Nil => Nil
      case h :: t => h :: stripTail(t)
    }
    implode(stripTail(stripHead(explode(src))))
  }

  @annotation.tailrec
  def checkChars(xs: List[Char], ts: List[Char]): Boolean =
    ts match {
      case Nil => true
      case h :: t => if (checkChar(xs, h)) checkChars(xs, t) else false
    }

  @annotation.tailrec
  def checkChar(xs: List[Char], ch: Char): Boolean =
    xs match {
      case h :: t => if (h == ch) true else checkChar(t, ch)
      case _ => false
    }

  def checkASCII(xs: List[Char]): Boolean =
    xs match {
      case h :: t => if (h.toInt <= 255) checkASCII(t) else false
      case _ => true
    }

  def implode(xs: List[Char]): String = {
    def loop(acc: String, n: Int): String =
      if (n >= xs.length) acc else loop(s"${acc}${xs(n)}", n + 1)
    loop("", 0)
  }

  def explode(src: String): List[Char] = {
    @annotation.tailrec
    def loop(acc: List[Char], n: Int): List[Char] =
      if (n <= 0) acc else loop(src(n - 1) :: acc, n - 1)
    loop(Nil, src.length)
  }

  def alphanums = digits ++ alphabets
  def digits = explode("0123456789")
  def lowerLetter = explode("abcdefghijklmnopqrstuvwxyz")
  def upperLetter = explode("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  def alphabets = lowerLetter ++ upperLetter
}
