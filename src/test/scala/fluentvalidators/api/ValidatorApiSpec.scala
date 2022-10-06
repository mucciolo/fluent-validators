package com.mucciolo
package fluentvalidators.api

import fluentvalidators.api.Validator
import fluentvalidators.api.Validator.Rule
import fluentvalidators.api.ValidatorApiSpec.*
import fluentvalidators.api.ValidatorApiSpec.Error.*
import fluentvalidators.api.impl.*
import fluentvalidators.syntax.ValidatorRuleSyntaxFor

import org.scalatest.*
import org.scalatest.Inspectors.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import org.scalatest.prop.*

final class ValidatorApiSpec extends AnyFlatSpec
  with should.Matchers with ValidatorRuleSyntaxFor[Error, Data] {

  private val positiveRule: Rule[Error, Data] = rule(_.positive > 0, NonPositiveInt)
  private val negativeRule: Rule[Error, Data] = rule(_.negative < 0, NonNegativeInt)
  private val zeroRule: Rule[Error, Data] = rule(_.zero == 0, NonZeroInt)

  "A sequential validator" should
    "be normalized to the same validator regardless of how it is declared" in {

    val normalizedValidator = Validator.of[Data]
      .withErrorTypeOf[Error]
      .seq(positiveRule, negativeRule, zeroRule)

    val unnormVal1 = Validator.of[Data]
      .withErrorTypeOf[Error]
      .seq(normalizedValidator)

    val positiveRuleValidator = Validator.of[Data]
      .withErrorTypeOf[Error]
      .seq(positiveRule)

    val negativeRuleValidator = Validator.of[Data]
      .withErrorTypeOf[Error]
      .seq(negativeRule)

    val zeroRuleValidator = Validator.of[Data]
      .withErrorTypeOf[Error]
      .seq(zeroRule)

    val unnormVal2 = Validator.of[Data]
      .withErrorTypeOf[Error]
      .seq(positiveRuleValidator, negativeRuleValidator, zeroRuleValidator)

    val posAndNegRuleValidator = Validator.of[Data]
      .withErrorTypeOf[Error]
      .seq(positiveRule, negativeRule)

    val unnormVal3 = Validator.of[Data]
      .withErrorTypeOf[Error]
      .seq(posAndNegRuleValidator, zeroRule)

    val unnormVal4 = Validator.of[Data]
      .withErrorTypeOf[Error]
      .seq(posAndNegRuleValidator, zeroRuleValidator)

    val negAndZeroRuleValidator = Validator.of[Data]
      .withErrorTypeOf[Error]
      .seq(negativeRule, zeroRule)

    val unnormVal5 = Validator.of[Data]
      .withErrorTypeOf[Error]
      .seq(positiveRule, negAndZeroRuleValidator)

    val unnormVal6 = Validator.of[Data]
      .withErrorTypeOf[Error]
      .seq(positiveRuleValidator, negAndZeroRuleValidator)

    val unnormalizedValidator =
      List(unnormVal1, unnormVal2, unnormVal3, unnormVal4, unnormVal5, unnormVal6)

    forAll(unnormalizedValidator)(_ should === (normalizedValidator))
  }

}

object ValidatorApiSpec {

  case class Data(positive: Int, negative: Int, zero: Int)

  sealed trait Error
  object Error {
    case object NonPositiveInt extends Error
    case object NonNegativeInt extends Error
    case object NonZeroInt extends Error
  }

}
