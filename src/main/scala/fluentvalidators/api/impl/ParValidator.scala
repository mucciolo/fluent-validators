package com.mucciolo
package fluentvalidators.api.impl

import fluentvalidators.api.{Rule, Validator}

import cats.Semigroup
import cats.data.{NonEmptyChain, ValidatedNec}
import cats.implicits.*
import cats.syntax.group.*

private[api] final case class ParValidator[+E, -A](validators: NonEmptyChain[Validator[E, A]])
  extends Validator[E, A] {

  override def validate[B <: A](instance: B): ValidatedNec[E, B] = {
    given semigroup: Semigroup[B] = Semigroup.first[B]

    validators.foldLeft(instance.validNec[E]) {
      (validated, validator) => validated |+| validator.validate(instance)
    }
  }

  override protected def parseSeqHeadValidator[EE >: E, B <: A](
    headValidator: Validator[EE, B]
  ): Validator[EE, B] = headValidator match {
    case _: EmptyValidator[EE, B] => this
    case validator: Validator[EE, B] => SeqValidator(this, validator)
  }

  override def narrow[B <: A]: Validator[E, B] = ParValidator[E, B](validators)

  override def contramap[B](f: B => A): Validator[E, B] = ParValidator(validators.map(_.contramap(f)))

}

private[api] object ParValidator {

  inline def apply[E, A](
    inline firstValidator: Validator[E, A],
    inline secondValidator: Validator[E, A],
    inline tailValidators: Validator[E, A]*
  ): ParValidator[E, A] =
    ParValidator(firstValidator +: NonEmptyChain.of(secondValidator, tailValidators: _*))

}