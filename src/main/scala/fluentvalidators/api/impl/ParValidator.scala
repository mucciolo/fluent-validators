package com.mucciolo
package fluentvalidators.api.impl

import fluentvalidators.api.{Rule, Validator}

import cats.Semigroup
import cats.data.{NonEmptyChain, ValidatedNec}
import cats.implicits.*
import cats.syntax.group.*

private[api] final case class ParValidator[+E, -A](validators: NonEmptyChain[Validator[E, A]])
  extends Validator[E, A] {

  override def validate[B <: A : Semigroup](instance: B): ValidatedNec[E, B] = {
    validators.foldLeft(instance.validNec[E]) {
      (validated, validator) => validated |+| validator.validate(instance)
    }
  }

  override protected def parseSeqHeadValidator[EE >: E, B <: A](
    headValidator: Validator[EE, B]
  ): Validator[EE, B] = {
    headValidator match {
      case rule: Rule[EE, B] => SeqValidator(this, rule)
      case seqValidator: SeqValidator[EE, B] => SeqValidator(this, seqValidator)
      case parValidator: ParValidator[EE, B] => SeqValidator(this, parValidator)
      case _: EmptyValidator[EE, B] => this
    }
  }

  override def par[EE >: E, B <: A](
    firstValidator : Validator[EE, B],
    secondValidator: Validator[EE, B],
    tailValidators : Validator[EE, B]*
  ): Validator[EE, B] = {
    SeqValidator(this, ParValidator(firstValidator, secondValidator, tailValidators: _*))
  }

  override def narrow[B <: A]: Validator[E, B] = {
    new ParValidator[E, B](validators)
  }

  override def contramap[B](f: B => A): Validator[E, B] = {
    new ParValidator(validators.map(_.contramap(f)))
  }

}

private[api] object ParValidator {

  inline def apply[E, A](
    inline firstValidator : Validator[E, A],
    inline secondValidator: Validator[E, A],
    inline tailValidators : Validator[E, A]*
  ): ParValidator[E, A] = {
    new ParValidator(firstValidator +: NonEmptyChain.of(secondValidator, tailValidators: _*))
  }

}