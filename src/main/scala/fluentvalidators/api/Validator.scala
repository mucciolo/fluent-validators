package com.mucciolo
package fluentvalidators.api

import fluentvalidators.api.impl.*

import cats.data.*

trait Validator[+E, -A] {

  def validate[B <: A](instance: B): ValidatedNec[E, B]

  def seq[EE >: E, B <: A](
    headValidator: Validator[EE, B],
    tailValidators: Validator[EE, B]*
  ): Validator[EE, B]

  def par[EE >: E, B <: A](
    firstValidator: Validator[EE, B],
    secondValidator: Validator[EE, B],
    tailValidators: Validator[EE, B]*
  ): Validator[EE, B]

  // TODO add error mapping
  def contramap[B](f: B => A): Validator[E, B]

}
