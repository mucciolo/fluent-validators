package com.mucciolo
package fluentvalidators

import fluentvalidators.matchers.ValidatedNecMatchers
import fluentvalidators.TestFixtures.*
import fluentvalidators.TestFixtures.Error.*
import fluentvalidators.api.*
import fluentvalidators.api.Validator.*
import fluentvalidators.api.Rule.*
import fluentvalidators.syntax.ValidatorRuleSyntaxFor

import cats.data.Validated
import cats.implicits.*
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.Inspectors.*
import org.scalatest.matchers.*
import org.scalatest.prop.*

final class ValidatorSpec extends AnyFlatSpec
                          with should.Matchers
                          with ValidatedNecMatchers
                          with ValidatorRuleSyntaxFor[Error, Data] {

  private val seqValidator: Validator[Error, Data] =
    Validator.of[Data]
      .withErrorTypeOf[Error]
      .seq(rule(_.negative < 0, NonNegativeInt), rule(_.positive > 0, NonPositiveInt))

  private val parValidator: Validator[Error, Data] =
    Validator.of[Data]
      .withErrorTypeOf[Error]
      .par(rule(_.negative < 0, NonNegativeInt), rule(_.positive > 0, NonPositiveInt))

  "A sequential validator" should "short-circuit" in {
    val data = Data(negative = +1, positive = +1)
    seqValidator.validate(data) should beInvalidDue(NonNegativeInt)
  }

  it should "test the next rule of a satisfied rule" in {
    val data = Data(negative = -1, positive = -1)
    seqValidator.validate(data) should beInvalidDue(NonPositiveInt)
  }

  it should "return the validated object when all rules are satisfied" in {
    val validData = Data(negative = -1, positive = +1)
    seqValidator.validate(validData) should beValid(validData)
  }

  it should "be normalized to the same validator regardless of how it is declared" in {

    val negativeRule: Rule[Error, Data] = rule(_.negative < 0, NonNegativeInt)
    val zeroRule: Rule[Error, Data] = rule(_.zero == 0, NonZeroInt)
    val positiveRule: Rule[Error, Data] = rule(_.positive > 0, NonPositiveInt)

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

    forAll(unnormalizedValidator)(_ should ===(normalizedValidator))
  }

  "A parallel validator" should "apply rules parallelly" in {
    val data = Data(negative = +1, positive = -1)
    parValidator.validate(data) should beInvalidDue(NonNegativeInt, NonPositiveInt)
  }

  it should "apply rules independently" in {

    val dataNonNegative = Data(negative = +1, positive = +1)
    parValidator.validate(dataNonNegative) should beInvalidDue(NonNegativeInt)

    val dataNonPositive = Data(negative = -1, positive = -1)
    parValidator.validate(dataNonPositive) should beInvalidDue(NonPositiveInt)

  }

  it should "return the validated object when all rules are satisfied" in {
    val validData = Data(negative = -1, positive = +1)
    parValidator.validate(validData) should beValid(validData)
  }

}
