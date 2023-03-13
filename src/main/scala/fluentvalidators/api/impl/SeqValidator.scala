package com.mucciolo
package fluentvalidators.api.impl

import fluentvalidators.api.{Rule, Validator}

import cats.Semigroup
import cats.data.*
import cats.implicits.catsSyntaxSemigroup
import cats.syntax.either.{catsSyntaxEither, catsSyntaxEitherIdBinCompat0}

private[api] final case class SeqValidator[+E, -A](validators: NonEmptyChain[Validator[E, A]])
  extends Validator[E, A] {

  override def validate[B <: A](instance: B): ValidatedNec[E, B] = {
    given semigroup: Semigroup[B] = Semigroup.first[B]

    validators.foldLeft(instance.rightNec[E]) {
      (validated: EitherNec[E, B], validator: Validator[E, A]) =>
        validated |+| validator.validate(instance).toEither
    }.toValidated
  }

  private def appendRule[EE >: E, B <: A](rule: Rule[EE, B]): SeqValidator[EE, B] = {

    val appendedValidators: NonEmptyChain[Validator[EE, B]] =
      this.validators.last match {

        case lastValidator: SeqValidator[E, A] =>
          NonEmptyChain.fromChainAppend(this.validators.init, lastValidator.appendRule(rule))

        case _ =>
          this.validators :+ rule

      }

    SeqValidator(appendedValidators)
  }

  override protected def parseSeqHeadValidator[EE >: E, B <: A](
    headValidator: Validator[EE, B]
  ): Validator[EE, B] = {

    headValidator match {
      case rule: Rule[EE, B] => this.appendRule(rule)
      case SeqValidator(otherValidators) => SeqValidator(this.validators ++ otherValidators)
      case parValidator: ParValidator[EE, B] => SeqValidator(this, parValidator)
      case _: EmptyValidator[EE, B] => this
    }
  }

  override def contramap[B](f: B => A): Validator[E, B] = SeqValidator(validators.map(_.contramap(f)))

}

private[api] object SeqValidator {
  inline def apply[E, A](
    inline headValidator: Validator[E, A],
    inline tailValidators: Validator[E, A]*
  ): SeqValidator[E, A] = {
    SeqValidator(NonEmptyChain.of(headValidator, tailValidators: _*))
  }
}