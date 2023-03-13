package com.mucciolo
package fluentvalidators

import fluentvalidators.TestFixtures.*
import fluentvalidators.TestFixtures.FieldError.*
import fluentvalidators.api.*
import fluentvalidators.api.syntax.*
import fluentvalidators.api.Validator.*
import fluentvalidators.matchers.ValidatedNecMatchers

import cats.data.Validated
import cats.implicits.*
import org.scalatest.*
import org.scalatest.Inspectors.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import org.scalatest.prop.*
import org.scalatest.wordspec.AnyWordSpec

final class CustomTypeValidatorSpec extends AnyWordSpec with should.Matchers with ValidatedNecMatchers {

  "A sequential validator" when {

    val seqValidator: Validator[FieldError, Data] =
      Validator.of[Data]
        .withErrorTypeOf[FieldError]
        .seq(
          rule(_.negative < 0, NonNegativeInt("negative")),
          rule(_.positive > 0, NonPositiveInt("positive"))
        )

    "validate" should {
      "short-circuit" in {
        val data = Data(negative = +1, positive = +1)
        seqValidator.validate(data) should beInvalidDue(NonNegativeInt("negative"))
      }

      "test the next rule of a satisfied rule" in {
        val data = Data(negative = -1, positive = -1)
        seqValidator.validate(data) should beInvalidDue(NonPositiveInt("positive"))
      }

      "return the validated object when all rules are satisfied" in {
        val validData = Data(negative = -1, positive = +1)
        seqValidator.validate(validData) should beValid(validData)
      }

      // TODO add more variations (seq.seq.seq) and move this to ValidatorApiSpec?
      "be normalized to the same validator regardless of how it is declared" in {

        val negativeRule: Rule[FieldError, Data] = rule(_.negative < 0, NonNegativeInt("negative"))
        val zeroRule: Rule[FieldError, Data] = rule(_.zero == 0, NonZeroInt("zero"))
        val positiveRule: Rule[FieldError, Data] = rule(_.positive > 0, NonPositiveInt("positive"))

        val normalizedValidator = Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(positiveRule, negativeRule, zeroRule)

        val unnormVal1 = Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(normalizedValidator)

        val positiveRuleValidator = Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(positiveRule)

        val negativeRuleValidator = Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(negativeRule)

        val zeroRuleValidator = Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(zeroRule)

        val unnormVal2 = Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(positiveRuleValidator, negativeRuleValidator, zeroRuleValidator)

        val posAndNegRuleValidator = Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(positiveRule, negativeRule)

        val unnormVal3 = Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(posAndNegRuleValidator, zeroRule)

        val unnormVal4 = Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(posAndNegRuleValidator, zeroRuleValidator)

        val negAndZeroRuleValidator = Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(negativeRule, zeroRule)

        val unnormVal5 = Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(positiveRule, negAndZeroRuleValidator)

        val unnormVal6 = Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(positiveRuleValidator, negAndZeroRuleValidator)

        val unnormalizedValidator =
          List(unnormVal1, unnormVal2, unnormVal3, unnormVal4, unnormVal5, unnormVal6)

        forAll(unnormalizedValidator)(_ should ===(normalizedValidator))
      }
    }
  }

  "A parallel validator" when {

    val parValidator: Validator[FieldError, Data] =
      Validator.of[Data]
        .withErrorTypeOf[FieldError]
        .par(
          rule(_.negative < 0, NonNegativeInt("negative")),
          rule(_.positive > 0, NonPositiveInt("positive"))
        )


    "validate" should {
     "apply rules parallelly" in {
        val data = Data(negative = +1, positive = -1)
        parValidator.validate(data) should beInvalidDue(NonNegativeInt("negative"), NonPositiveInt("positive"))
      }

      "apply rules independently" in {

        val dataNonNegative = Data(negative = +1, positive = +1)
        parValidator.validate(dataNonNegative) should beInvalidDue(NonNegativeInt("negative"))

        val dataNonPositive = Data(negative = -1, positive = -1)
        parValidator.validate(dataNonPositive) should beInvalidDue(NonPositiveInt("positive"))

      }

      "return the validated object when all rules are satisfied" in {
        val validData = Data(negative = -1, positive = +1)
        parValidator.validate(validData) should beValid(validData)
      }
    }
  }

}
