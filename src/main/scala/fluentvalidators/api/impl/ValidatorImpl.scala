package com.mucciolo
package fluentvalidators.api.impl

import fluentvalidators.api.Validator

private[api] trait ValidatorImpl[+E, -A] extends Validator[E, A] {

  protected def parseSeqHeadValidator[EE >: E, B <: A](
    headValidator: Validator[EE, B]
  ): Validator[EE, B]

  override def seq[EE >: E, B <: A](
    headValidator: Validator[EE, B],
    tailValidators: Validator[EE, B]*
  ): Validator[EE, B] = {

    val parsedHeadValidator: Validator[EE, B] = parseSeqHeadValidator(headValidator)

    tailValidators match {
      case head +: tail => parsedHeadValidator.seq(head, tail: _*)
      case _ => parsedHeadValidator
    }
  }

  override def par[EE >: E, B <: A](
    firstValidator: Validator[EE, B],
    secondValidator: Validator[EE, B],
    tailValidators: Validator[EE, B]*
  ): Validator[EE, B] =
    SeqValidator(this, ParValidator(firstValidator, secondValidator, tailValidators: _*))

}
