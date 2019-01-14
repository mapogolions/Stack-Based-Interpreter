package io.github.mapogolions.cs305.buffalo

import io.github.mapogolions.cs305.buffalo.Vals


/*
type ('k 'v) env= {
  parent : null | ('k 'v) env ;
  bindings : ('k 'v) map
}
 */
trait Env

case object NULLABLE extends Env
case class Scope(val bindings: Map[String, Vals]) extends Env
