package com.mucciolo
package fluentvalidators.api.impl

import fluentvalidators.api.{Rule, Validator}

import cats.Semigroup
import cats.data.ValidatedNec
import cats.implicits.*

private[api] final case class SingletonValidator[+E, -A](predicate: A => Boolean, caseFalse: E)
  extends ValidatorImpl[E, A] with Rule[E, A] {

  override def validate[B <: A](instance: B): ValidatedNec[E, B] =
    if (predicate(instance)) instance.validNec else caseFalse.invalidNec

  override protected def parseSeqHeadValidator[EE >: E, B <: A](
    headValidator: Validator[EE, B]
  ): Validator[EE, B] = headValidator match {

    case ruleOrParValidator: (Rule[EE, B] | ParValidator[EE, B]) =>
      SeqValidator(this, ruleOrParValidator)

    case SeqValidator(validators) => SeqValidator(this +: validators)

    case _: EmptyValidator[EE, B] => this

  }

  override def contramap[B](f: B => A): Rule[E, B] = SingletonValidator(predicate.compose(f), caseFalse)

}