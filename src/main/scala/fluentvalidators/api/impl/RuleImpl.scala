package com.mucciolo
package fluentvalidators.api.impl

import fluentvalidators.api.{Rule, Validator}

import cats.Semigroup
import cats.data.ValidatedNec
import cats.implicits.*

private[api] final case class RuleImpl[+E, -A](predicate: A => Boolean, caseFalse: E)
  extends Rule[E, A] {

  override def validate[B <: A : Semigroup](instance: B): ValidatedNec[E, B] =
    if (predicate(instance)) instance.validNec else caseFalse.invalidNec

  override protected def parseSeqHeadValidator[EE >: E, B <: A](
    headValidator: Validator[EE, B]
  ): Validator[EE, B] = {
    headValidator match {
      case rule: Rule[EE, B] => SeqValidator(this, rule)
      case SeqValidator(validators) => new SeqValidator(this +: validators)
      case parValidator: ParValidator[EE, B] => SeqValidator(this, parValidator)
      case EmptyValidator() => this
    }
  }

  override def par[EE >: E, B <: A](
    headValidator : Validator[EE, B],
    tailValidators: Validator[EE, B]*
  ): Validator[EE, B] = {
    SeqValidator(this, ParValidator(headValidator, tailValidators: _*))
  }

  override def narrow[B <: A]: Rule[E, B] = {
    RuleImpl(predicate, caseFalse)
  }

  override def contramap[B](f: B => A): Rule[E, B] = {
    RuleImpl(predicate.compose(f), caseFalse)
  }

}