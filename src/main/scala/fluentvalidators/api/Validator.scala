package com.mucciolo
package fluentvalidators.api

import fluentvalidators.api.Validator.*
import fluentvalidators.api.impl.*

import cats.Semigroup
import cats.data.*
import cats.implicits.*
import cats.syntax.*

import scala.annotation.tailrec

// TODO fluentvalidators.syntax
trait Validator[+E, -A] {

  def validate[B <: A](instance: B): ValidatedNec[E, B]

  protected def parseSeqHeadValidator[EE >: E, B <: A](
    headValidator: Validator[EE, B]
  ): Validator[EE, B]

  def seq[EE >: E, B <: A](
    headValidator: Validator[EE, B],
    tailValidators: Validator[EE, B]*
  ): Validator[EE, B] = {

    val parsedHeadValidator: Validator[EE, B] = parseSeqHeadValidator(headValidator)

    tailValidators match {
      case head +: tail => parsedHeadValidator.seq(head, tail: _*)
      case _ => parsedHeadValidator
    }
  }

  // TODO consider having a default implementation and override on EmptyValidator
  def par[EE >: E, B <: A](
    firstValidator: Validator[EE, B],
    secondValidator: Validator[EE, B],
    tailValidators: Validator[EE, B]*
  ): Validator[EE, B]

  // TODO consider the necessity of a widenError method
  def narrow[B <: A]: Validator[E, B]

  // TODO add error mapping
  def contramap[B](f: B => A): Validator[E, B]

}

object Validator {
  inline def of[A] = new ValidatorBuilder[A]()
}

private final class ValidatorBuilder[-A]() {
  inline def withErrorTypeOf[E]: Validator[E, A] = new EmptyValidator[E, A]()
}
