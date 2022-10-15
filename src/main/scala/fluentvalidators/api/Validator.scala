package com.mucciolo
package fluentvalidators.api

import fluentvalidators.api.Validator.*
import fluentvalidators.api.impl.*

import cats.Semigroup
import cats.data.*
import cats.implicits.*
import cats.syntax.*

import scala.annotation.tailrec

trait Validator[+E, -A] {

  def validate[B <: A](instance: B)
    (using semigroup: Semigroup[B] = Semigroup.first[B]): ValidatedNec[E, B]

  protected def parseSeqHeadValidator[EE >: E, B <: A](
    headValidator: Validator[EE, B]
  ): Validator[EE, B]

  def seq[EE >: E, B <: A](
    headValidator : Validator[EE, B],
    tailValidators: Validator[EE, B]*
  ): Validator[EE, B] = {

    val parsedHeadValidator = parseSeqHeadValidator(headValidator)

    tailValidators match {
      case head +: tail => parsedHeadValidator.seq(head, tail: _*)
      case _ => parsedHeadValidator
    }
  }

  // TODO add second validator
  def par[EE >: E, B <: A](
    headValidator : Validator[EE, B],
    tailValidators: Validator[EE, B]*): Validator[EE, B]

  def narrow[B <: A]: Validator[E, B]

  def contramap[B](f: B => A): Validator[E, B]

}

private final class ValidatorBuilder[-A]() {
  inline def withErrorTypeOf[E]: Validator[E, A] = new EmptyValidator[E, A]()
}

object Validator {
  inline def of[A] = new ValidatorBuilder[A]()
}