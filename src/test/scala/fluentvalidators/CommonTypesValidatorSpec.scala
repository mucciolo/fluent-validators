package com.mucciolo
package fluentvalidators

import fluentvalidators.TestFixtures.SimpleError
import fluentvalidators.TestFixtures.SimpleError.*
import fluentvalidators.api.Rule.rule
import fluentvalidators.api.Validator
import fluentvalidators.matchers.ValidatedNecMatchers

import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

final class CommonTypesValidatorSpec extends AnyWordSpec
  with should.Matchers with ValidatedNecMatchers {

  "String validator" when {
    "empty" should {
      "always return validated instance" in {
        val emptyValidator = Validator.of[String].withErrorTypeOf[SimpleError]

        emptyValidator.validate("") should beValid("")
        emptyValidator.validate("abc") should beValid("abc")
        emptyValidator.validate("a1 b2 c3") should beValid("a1 b2 c3")
      }
    }

    "composed of a single sequential rule" should {

      val validator: Validator[SimpleError, String] =
        Validator.of[String].withErrorTypeOf[SimpleError]
          .seq(rule(!_.isBlank, EmptyString))

      "return validated instance when rule is satisfied" in {
        validator.validate("str") should beValid("str")
      }

      "return error when rule is violated" in {
        validator.validate("") should beInvalidDue(EmptyString)
      }
    }

    "composed of two parallel rules" should {

      val validator: Validator[SimpleError, String] =
        Validator.of[String].withErrorTypeOf[SimpleError]
          .par(
            rule(!_.contains("*"), InvalidChars),
            rule(_.length <= 2, TooLong(2))
          )

      "return validated instance when both rules are satisfied" in {
        validator.validate("12") should beValid("12")
      }

      "return two errors when both rules are violated" in {
        validator.validate("12*") should beInvalidDue(InvalidChars, TooLong(2))
      }
    }

    "composed of two sequential rules" should {

      val validator: Validator[SimpleError, String] =
        Validator.of[String]
          .withErrorTypeOf[SimpleError]
          .seq(
            rule(!_.contains("*"), InvalidChars),
            rule(_.length <= 2, TooLong(2))
          )

      "return validated instance when both rules are satisfied" in {
        validator.validate("12") should beValid("12")
      }

      "return first declared error" in {
        validator.validate("12*") should beInvalidDue(InvalidChars)
      }
      "return second declared error" in {
        validator.validate("123") should beInvalidDue(TooLong(2))
      }
    }
  }

  "Int validator" when {
    "composed of sequential followed by parallel validator" should {

      val validator: Validator[SimpleError, Int] =
        Validator.of[Int].withErrorTypeOf[SimpleError]
          .seq(rule(_ % 2 == 0, NotEven))
          .par(
            rule(_ >= 0, NegativeInt),
            rule(_ <= 10, GreaterThan(10))
          )

      "return validated instance when all rules are satisfied" in {
        validator.validate(2) should beValid(2)
      }

      "short-circuit when the sequential validator rules are violated" in {
        validator.validate(1) should beInvalidDue(NotEven)
      }

      "check both rules parallel rules when sequential validator rules are satisfied" in {
        validator.validate(-2) should beInvalidDue(NegativeInt)
        validator.validate(12) should beInvalidDue(GreaterThan(10))
      }

    }

    "composed of parallel followed by sequential validator" should {

      val validator: Validator[SimpleError, Int] =
        Validator.of[Int]
          .withErrorTypeOf[SimpleError]
          .par(
            rule(_ >= 0, NegativeInt),
            rule(_ <= 10, GreaterThan(10)))
          .seq(
            rule(_ % 2 == 0, NotEven))

      "return validated instance when all rules are satisfied" in {
        validator.validate(2) should beValid(2)
      }

      "short-circuit when the parallel validator rules are violated" in {
        validator.validate(-2) should beInvalidDue(NegativeInt)
        validator.validate(12) should beInvalidDue(GreaterThan(10))
      }

      "check sequential validator when parallel rules are satisfied" in {
        validator.validate(1) should beInvalidDue(NotEven)
      }

    }
  }

}
