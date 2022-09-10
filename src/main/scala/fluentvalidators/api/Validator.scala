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

  def seq[EE >: E, B <: A](headRuleOrValidator: RuleOrValidator[EE, B],
                           tailRuleOrValidators: RuleOrValidator[EE, B]*): Validator[EE, B]

  def par[EE >: E, B <: A](headRuleOrValidator: RuleOrValidator[EE, B],
                           tailRuleOrValidators: RuleOrValidator[EE, B]*): Validator[EE, B]

  def narrow[B <: A]: Validator[E, B]

  def contramap[B](f: B => A): Validator[E, B]

}

private final class ValidatorBuilder[-A]() {
  inline def withErrorTypeOf[E]: Validator[E, A] = new EmptyValidator[E, A]()
}

object Validator {

  type Rule[+E, -A] = [B <: A] => B => ValidatedNec[E, B]
  type RuleOrValidator[+E, -A] = Rule[E, A] | Validator[E, A]

  inline def of[A] = new ValidatorBuilder[A]()

  inline def rule[E, A](inline predicate: A => Boolean, inline caseFalse: E): Rule[E, A] = {
    val caseFalseInvalidNec = caseFalse.invalidNec
    [B <: A] => (instance: B) => if (predicate(instance)) instance.validNec else caseFalseInvalidNec
  }

  extension [E, A] (rule: Rule[E, A]) {
    def contramap[B](f: B => A): Rule[E, B] = {
      [T <: B] => (b: T) => rule.apply(f(b)).map(_ => b)
    }
  }

  extension [E, A] (ruleOrValidator: RuleOrValidator[E, A]) {
    def toValidator: Validator[E, A] = {
      ruleOrValidator match {
        case rule: Rule[E, A] => SeqValidator[E, A](rule)
        case validator: Validator[E, A] => validator
      }
    }
  }

}

package impl {

  import Validator.*

  private[api] final case class EmptyValidator[+E, -A]() extends Validator[E, A] {

    override def validate[B <: A : Semigroup](instance: B): ValidatedNec[E, B] = {
      instance.validNec
    }

    override def seq[EE >: E, B <: A](
      headRuleOrValidator: RuleOrValidator[EE, B],
      tailRuleOrValidators: RuleOrValidator[EE, B]*): Validator[EE, B] = {

      val headValidator = headRuleOrValidator.toValidator

      tailRuleOrValidators match {
        case head +: tail => headValidator.seq(head, tail: _*)
        case _ => headValidator
      }
    }

    override def par[EE >: E, B <: A]
      (headRuleOrValidator: RuleOrValidator[EE, B],
       tailRuleOrValidators: RuleOrValidator[EE, B]*) : Validator[EE, B] = {
      ParValidator(headRuleOrValidator, tailRuleOrValidators: _*)
    }

    override def narrow[B <: A]: Validator[E, B] = {
      new EmptyValidator[E, B]()
    }

    override def contramap[B](f: B => A): Validator[E, B] = {
      new EmptyValidator[E, B]()
    }

  }

  private[api] final case class SeqValidator[+E, -A](rules: NonEmptyChain[Rule[E, A]])
    extends Validator[E, A] {

    override def validate[B <: A : Semigroup](instance: B): ValidatedNec[E, B] = {
      rules.foldLeft(instance.asRight[NonEmptyChain[E]]) {
        (validated: EitherNec[E, B], rule: Rule[E, A]) => validated |+| rule.apply(instance).toEither
      }.toValidated
    }

    override def seq[EE >: E, B <: A](
      headRuleOrValidator: RuleOrValidator[EE, B],
      tailRuleOrValidators: RuleOrValidator[EE, B]*): Validator[EE, B] = {

      val headValidator = headRuleOrValidator match {
        case rule: Rule[EE, B] =>
          new SeqValidator(this.rules :+ rule)

        case seqValidator: SeqValidator[EE, B] =>
          new SeqValidator(this.rules ++ seqValidator.rules)

        case parValidator: ParValidator[EE, B] =>
          ValidatorChain(this, parValidator)

        case validatorChain: ValidatorChain[EE, B] =>
          new ValidatorChain(this +: validatorChain.validators)

        case _: EmptyValidator[EE, B] =>
          this
      }

      tailRuleOrValidators match {
        case head +: tail => headValidator.seq(head, tail: _*)
        case _ => headValidator
      }
    }

    override def par[EE >: E, B <: A](
      headRuleOrValidator: RuleOrValidator[EE, B],
      tailRuleOrValidators: RuleOrValidator[EE, B]*): Validator[EE, B] = {
      ValidatorChain(this, ParValidator(headRuleOrValidator, tailRuleOrValidators: _*))
    }

    override def narrow[B <: A]: Validator[E, B] = {
      new SeqValidator[E, B](rules)
    }

    override def contramap[B](f: B => A): Validator[E, B] = {
      new SeqValidator(rules.map((rule: Rule[E, A]) => rule.contramap(f)))
    }

  }

  private[api] object SeqValidator {
    inline def apply[E, A](inline headRule: Rule[E, A],
                           inline tailRules: Rule[E, A]*): SeqValidator[E, A] = {
      new SeqValidator(NonEmptyChain.of(headRule, tailRules: _*))
    }
  }

  private[api] final case class ParValidator[+E, -A](validators: NonEmptyChain[Validator[E, A]])
    extends Validator[E, A] {

    override def validate[B <: A : Semigroup](instance: B): ValidatedNec[E, B] = {
      validators.foldLeft(instance.validNec[E]) {
        (validated, validator) => validated |+| validator.validate(instance)
      }
    }

    override def seq[EE >: E, B <: A](headRuleOrValidator: RuleOrValidator[EE, B],
                                      tailRuleOrValidators: RuleOrValidator[EE, B]*)
    : Validator[EE, B] = {

      val headValidator = headRuleOrValidator match {
        case rule: Rule[EE, B] =>
          ValidatorChain(this, rule.toValidator)

        case seqValidator: SeqValidator[EE, B] =>
          ValidatorChain(this, seqValidator)

        case parValidator: ParValidator[EE, B] =>
          ValidatorChain(this, parValidator)

        case validatorChain: ValidatorChain[EE, B] =>
          new ValidatorChain(this +: validatorChain.validators)

        case _: EmptyValidator[EE, B] =>
          this
      }

      tailRuleOrValidators match {
        case head +: tail => headValidator.seq(head, tail: _*)
        case _ => headValidator
      }
    }

    override def par[EE >: E, B <: A](headRuleOrValidator: RuleOrValidator[EE, B],
                                      tailRuleOrValidators: RuleOrValidator[EE, B]*)
    : Validator[EE, B] = {
      ValidatorChain(this, ParValidator(headRuleOrValidator, tailRuleOrValidators: _*))
    }

    override def narrow[B <: A]: Validator[E, B] = {
      new ParValidator[E, B](validators)
    }

    override def contramap[B](f: B => A): Validator[E, B] = {
      new ParValidator(validators.map(_.contramap(f)))
    }

  }

  private[api] object ParValidator {

    inline def apply[E, A](inline headValidator: Validator[E, A],
                           inline tailValidators: Validator[E, A]*): ParValidator[E, A] = {
      new ParValidator(NonEmptyChain.of(headValidator, tailValidators: _*))
    }

    inline def apply[E, A](inline headRuleOrValidator: RuleOrValidator[E, A],
                           inline tailRuleOrValidators: RuleOrValidator[E, A]*): ParValidator[E, A] = {

      val headValidator: Validator[E, A] = headRuleOrValidator.toValidator
      val tailValidators: Seq[Validator[E, A]] = tailRuleOrValidators.map(_.toValidator)

      ParValidator(headValidator, tailValidators: _*)
    }

  }

  private[api] final case class ValidatorChain[+E, -A](validators: NonEmptyChain[Validator[E, A]])
    extends Validator[E, A] {

    override def validate[B <: A : Semigroup](instance: B): ValidatedNec[E, B] = {
      validators.foldLeft(instance.asRight[NonEmptyChain[E]]) {
        (validated, validator) => validated |+| validator.validate(instance).toEither
      }.toValidated
    }

    override def seq[EE >: E, B <: A](
      headRuleOrValidator: RuleOrValidator[EE, B],
      tailRuleOrValidators: RuleOrValidator[EE, B]*): Validator[EE, B] = {

      validators.init

      // TODO squash rule and sequential validators
      val headValidator = headRuleOrValidator match {
        case rule: Rule[EE, B] =>
          new ValidatorChain(this.validators :+ rule.toValidator)

        case seqOrParValidator: (SeqValidator[EE, B] | ParValidator[EE, B]) =>
          new ValidatorChain(this.validators :+ seqOrParValidator)

        case validatorChain: ValidatorChain[EE, B] =>
          new ValidatorChain(this.validators ++ validatorChain.validators)

        case _: EmptyValidator[EE, B] =>
          this
      }


      tailRuleOrValidators match {
        case head +: tail => headValidator.seq(head, tail: _*)
        case _ => headValidator
      }
    }

    override def par[EE >: E, B <: A](headRuleOrValidator: RuleOrValidator[EE, B],
                                      tailRuleOrValidators: RuleOrValidator[EE, B]*)
    : Validator[EE, B] = {
      new ValidatorChain(validators :+ ParValidator(headRuleOrValidator, tailRuleOrValidators: _*))
    }

    override def narrow[B <: A]: Validator[E, B] = {
      new ValidatorChain[E, B](validators)
    }

    override def contramap[B](f: B => A): Validator[E, B] = {
      new ValidatorChain(validators.map(_.contramap(f)))
    }

  }

  private[api] object ValidatorChain {
    inline def apply[E, A](inline firstValidator: Validator[E, A],
                           inline secondValidator: Validator[E, A],
                           inline tailValidators: Validator[E, A]*): ValidatorChain[E, A] = {
      new ValidatorChain(
        NonEmptyChain.one(firstValidator) ++ NonEmptyChain.of(secondValidator, tailValidators: _*)
      )
    }

  }
}