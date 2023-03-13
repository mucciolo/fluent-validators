package com.mucciolo
package fluentvalidators.api

import fluentvalidators.api.impl.*

import cats.data.*

import scala.annotation.tailrec

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

  def par[EE >: E, B <: A](
    firstValidator: Validator[EE, B],
    secondValidator: Validator[EE, B],
    tailValidators: Validator[EE, B]*
  ): Validator[EE, B] =
    SeqValidator(this, ParValidator(firstValidator, secondValidator, tailValidators: _*))

  // TODO add error mapping
  def contramap[B](f: B => A): Validator[E, B]

}
