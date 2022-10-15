package com.mucciolo
package fluentvalidators.api

import fluentvalidators.api.impl.RuleImpl

trait Rule[+E, -A] extends Validator[E, A] {
  override def narrow[B <: A]: Rule[E, B]
  override def contramap[B](f: B => A): Rule[E, B]
}

object Rule {
  def rule[E, A](predicate: A => Boolean, caseFalse: E): Rule[E, A] = RuleImpl(predicate, caseFalse)
}