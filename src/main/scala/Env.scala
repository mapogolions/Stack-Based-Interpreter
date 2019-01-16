package io.github.mapogolions.cs305.buffalo

import io.github.mapogolions.cs305.buffalo.Vals._


/*
type ('k 'v) env= {
  parent : null | ('k 'v) env ;
  bindings : ('k 'v) map
}
 */
trait Env { self =>
  def add(pair: (String, Vals)) = self match {
    case Empty => Scope(Map(pair), Empty)
    case Scope(binds, parent) => Scope(binds + pair, parent)
  }

  @annotation.tailrec
  final def get(name: String): Option[Vals] = self match {
    case Empty => None
    case Scope(binds, _) if (binds contains name) => binds get name
    case Scope(_, parent) => parent get name
  }

  @annotation.tailrec
  final def has(name: String): Boolean =  self match {
    case Empty => false
    case Scope(binds, _) if (binds contains name) => true
    case Scope(_, parent) => parent has name
  }

  def binds = self match {
    case Empty => Map()
    case Scope(binds, _) => binds
  }

  def parent = self match {
    case Empty => Empty
    case Scope(_, parent) => parent
  }
}

case object Empty extends Env
case class Scope(
  override val binds: Map[String, Vals], 
  override val parent: Env
) extends Env