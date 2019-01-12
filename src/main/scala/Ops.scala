package io.github.mapogolions.cs.buffalo

import io.github.mapogolions.cs.buffalo._


object Ops {
  import Vals._

  // I stoped here. Need implement some additional branches
  // def parseLiteral(src: String): Vals =
  //   explode(src) match {
  //     case '-' :: t => parseInt(implode(t), '-')
  //     case '"' :: t => if (checkASCII(t)) STRING(stripQuetes(s)) else ERROR
  //     case ':' :: t => parseBool(s)
  //     case _ => ERROR
  //   }

  // def stripQuotes(src: String): String = ???

  def parseID(src: String): Vals =
    explode(src) match {
      case h :: t if (checkChar(alphabets, h) && checkChars(alphanums, t)) => ID(src)
      case _ => ERROR
    }

  def parseInt(src: String, sign: Char): Vals =
    if (checkChars(digits, src.toList)) INT(sign, src.toInt)
    else ERROR

  def parseBool(src: String): Vals = src.trim match {
    case ":true:" => BOOL(true)
    case ":false:" => BOOL(false)
    case _ => ERROR
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
      if (n <= 0) src(n) :: acc else loop(src(n) :: acc, n - 1)
    loop(Nil, src.length - 1)
  }

  def alphanums = digits ++ alphabets
  def digits = explode("0123456789")
  def lowerLetter = explode("abcdefghijklmnopqrstuvwxyz")
  def upperLetter = explode("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  def alphabets = lowerLetter ++ upperLetter
}
