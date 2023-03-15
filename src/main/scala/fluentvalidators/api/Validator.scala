package com.mucciolo
package fluentvalidators.api

import fluentvalidators.api.impl.*

import cats.data.*

/**
  * @tparam E error
  * @tparam A validatee
  */
trait Validator[+E, -A] {

  /**
    * Performs the validation of the given [[instance]] according to the rules of this validator.
    */
  def validate[B <: A](instance: B): ValidatedNec[E, B]

  /**
    * Applies the given validators in order, short-circuiting the validation on the first failure.
    */
  def seq[EE >: E, B <: A](
    headValidator: Validator[EE, B],
    tailValidators: Validator[EE, B]*
  ): Validator[EE, B]

  /**
    * Applies the given validators in parallel, accumulating their errors.
    */
  def par[EE >: E, B <: A](
    firstValidator: Validator[EE, B],
    secondValidator: Validator[EE, B],
    tailValidators: Validator[EE, B]*
  ): Validator[EE, B]

  /**
    * A contramap in the validatee type and a map in the error type.
    * @note Terminology borrowed from profunctors.
    */
  def dimap[AA, F](f: AA => A, g: E => F): Validator[F, AA]

}
