package com.mucciolo
package fluentvalidators.api.impl

import fluentvalidators.api.{Rule, Validator}

import cats.Semigroup
import cats.data.ValidatedNec
import cats.implicits.*

private[api] final case class EmptyValidator[+E, -A]() extends Validator[E, A] {

  override def validate[B <: A](instance: B): ValidatedNec[E, B] = {
    instance.validNec
  }

  override protected def parseSeqHeadValidator[EE >: E, B <: A](
    headValidator: Validator[EE, B]
  ): Validator[EE, B] = {
    headValidator match
      case rule: Rule[EE, B] => SeqValidator(rule)
      case _ => headValidator
  }

  override def par[EE >: E, B <: A](
    firstValidator: Validator[EE, B],
    secondValidator: Validator[EE, B],
    tailValidators: Validator[EE, B]*
  ): Validator[EE, B] = {
    ParValidator(firstValidator, secondValidator, tailValidators: _*)
  }

  override def narrow[B <: A]: Validator[E, B] = {
    EmptyValidator[E, B]()
  }

  override def contramap[B](f: B => A): Validator[E, B] = {
    EmptyValidator[E, B]()
  }

}