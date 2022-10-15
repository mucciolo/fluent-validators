package com.mucciolo
package fluentvalidators.api.impl

import fluentvalidators.api.{Rule, Validator}

import cats.Semigroup
import cats.data.{EitherNec, NonEmptyChain, ValidatedNec}
import cats.implicits.*
import cats.syntax.group.*

private[api] final case class SeqValidator[+E, -A](validators: NonEmptyChain[Validator[E, A]])
  extends Validator[E, A] {

  override def validate[B <: A : Semigroup](instance: B): ValidatedNec[E, B] = {
    validators.foldLeft(instance.asRight[NonEmptyChain[E]]) {
      (validated: EitherNec[E, B], validator: Validator[E, A]) =>
        validated |+| validator.validate(instance).toEither
    }.toValidated
  }

  override protected def parseSeqHeadValidator[EE >: E, B <: A](
    headValidator: Validator[EE, B]): Validator[EE, B] = {
    headValidator match {
      case rule: Rule[EE, B] => new SeqValidator(
        this.validators.last match {
          case lastValidator: SeqValidator[E, A] =>
            NonEmptyChain.fromChainAppend(
              this.validators.init,
              new SeqValidator(lastValidator.validators :+ rule)
            )
          case _ =>
            this.validators :+ rule
        }
      )
      case SeqValidator(otherValidators) => new SeqValidator(this.validators ++ otherValidators)
      case parValidator: ParValidator[EE, B] => SeqValidator(this, parValidator)
      case _: EmptyValidator[EE, B] => this
    }
  }

  override def par[EE >: E, B <: A](
    headValidator : Validator[EE, B],
    tailValidators: Validator[EE, B]*
  ): Validator[EE, B] = {
    SeqValidator(this, ParValidator(headValidator, tailValidators: _*))
  }

  override def narrow[B <: A]: Validator[E, B] = {
    new SeqValidator[E, B](validators)
  }

  override def contramap[B](f: B => A): Validator[E, B] = {
    new SeqValidator(validators.map(_.contramap(f)))
  }

}

private[api] object SeqValidator {
  inline def apply[E, A](
    inline headValidator : Validator[E, A],
    inline tailValidators: Validator[E, A]*
  ): SeqValidator[E, A] = {
    new SeqValidator(NonEmptyChain.of(headValidator, tailValidators: _*))
  }
}