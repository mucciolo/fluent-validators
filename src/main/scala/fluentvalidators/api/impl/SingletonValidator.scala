package com.mucciolo
package fluentvalidators.api.impl

import fluentvalidators.api.*
import fluentvalidators.api.syntax.*

import cats.Semigroup
import cats.data.ValidatedNec
import cats.implicits.*

private[api] final case class SingletonValidator[+E, -A](predicate: A => Boolean, caseFalse: E)
  extends Rule[E, A] with ValidatorImpl[E, A] {

  override def validate[B <: A](instance: B): ValidatedNec[E, B] =
    if (predicate(instance)) instance.validNec else caseFalse.invalidNec

  override protected def parseSeqHeadValidator[EE >: E, B <: A](
    headValidator: Validator[EE, B]
  ): Validator[EE, B] = headValidator match {

    case ruleOrParValidator: (Rule[EE, B] | ParValidator[EE, B]) =>
      SeqValidator(this, ruleOrParValidator)

    case seqValidator: SeqValidator[EE, B] =>
      seqValidator.preppendRule(this)

    case _: EmptyValidator[EE, B] =>
      this

  }

  override def dimap[B, F](f: B => A, g: E => F): SingletonValidator[F, B] =
    SingletonValidator(f.andThen(predicate), g(caseFalse))

}