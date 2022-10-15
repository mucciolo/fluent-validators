package com.mucciolo
package fluentvalidators.api

import fluentvalidators.api.Validator.*
import fluentvalidators.api.impl.*

import cats.Semigroup
import cats.data.*
import cats.implicits.*
import cats.syntax.*

sealed trait Validator[+E, -A] {

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

sealed trait Rule[+E, -A] extends Validator[E, A] {
  override def narrow[B <: A]: Rule[E, B]
  override def contramap[B](f: B => A): Rule[E, B]
}

object Rule {
  def rule[E, A](predicate: A => Boolean, caseFalse: E): Rule[E, A] = RuleImpl(predicate, caseFalse)
}

package impl {

  import Validator.*

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

  private[api] final case class EmptyValidator[+E, -A]() extends Validator[E, A] {

    override def validate[B <: A : Semigroup](instance: B): ValidatedNec[E, B] = {
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
      headValidator : Validator[EE, B],
      tailValidators: Validator[EE, B]*
    ): Validator[EE, B] = {
      ParValidator(headValidator, tailValidators: _*)
    }

    override def narrow[B <: A]: Validator[E, B] = {
      new EmptyValidator[E, B]()
    }

    override def contramap[B](f: B => A): Validator[E, B] = {
      new EmptyValidator[E, B]()
    }

  }

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
      headValidator : Validator[EE, B],
      tailValidators: Validator[EE, B]*
    ): Validator[EE, B] = {
      SeqValidator(this, ParValidator(headValidator, tailValidators: _*))
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
      inline headValidator : Validator[E, A],
      inline tailValidators: Validator[E, A]*
    ): ParValidator[E, A] = {
      new ParValidator(NonEmptyChain.of(headValidator, tailValidators: _*))
    }

  }

}