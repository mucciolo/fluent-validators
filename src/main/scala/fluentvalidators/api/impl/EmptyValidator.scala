package com.mucciolo
package fluentvalidators.api.impl

import fluentvalidators.api.*
import fluentvalidators.api.syntax.*

import cats.Semigroup
import cats.data.ValidatedNec
import cats.implicits.*

private[api] final case class EmptyValidator[+E, -A]() extends ValidatorImpl[E, A] {

  override def validate[B <: A](instance: B): ValidatedNec[E, B] = instance.validNec

  override protected def parseSeqHeadValidator[EE >: E, B <: A](
    headValidator: Validator[EE, B]
  ): Validator[EE, B] =
    headValidator

  override def par[EE >: E, B <: A](
    firstValidator: Validator[EE, B],
    secondValidator: Validator[EE, B],
    tailValidators: Validator[EE, B]*
  ): Validator[EE, B] =
    ParValidator(firstValidator, secondValidator, tailValidators: _*)

  override def dimap[B, F](f: B => A, g: E => F): Validator[F, B] = EmptyValidator[F, B]()

}