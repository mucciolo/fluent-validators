package com.mucciolo
package fluentvalidators.api

import fluentvalidators.TestFixtures.*
import fluentvalidators.TestFixtures.FieldError.*
import fluentvalidators.api.syntax.*
import fluentvalidators.api.Validator
import fluentvalidators.api.syntax.*
import fluentvalidators.api.impl.*

import org.scalatest.*
import org.scalatest.Inspectors.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import org.scalatest.prop.*
import org.scalatest.wordspec.AnyWordSpec

final class ApiImplSpec extends AnyWordSpec with should.Matchers {

  "Validator" when {

    "just created" should {
      "be empty" in {
        Validator.of[Data].withErrorTypeOf[FieldError] shouldBe an[EmptyValidator[_, _]]
      }
    }

    "seq" should {
      "build a singleton validator given a single rule" in {
        Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(rule(_.zero == 0, NonZeroInt("zero"))) shouldBe a[SingletonValidator[_, _]]
      }

      "build a sequential validator given two rules" in {
        Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(
            rule(_.negative < 0, NonNegativeInt("negative")),
            rule(_.positive > 0, NonPositiveInt("positive"))
          ) shouldBe a[SeqValidator[_, _]]
      }
    }

    "par" should {
      "build a parallel validator" in {
        Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .par(
            rule(_.negative < 0, NonNegativeInt("negative")),
            rule(_.positive > 0, NonPositiveInt("positive"))
          ) shouldBe a[ParValidator[_, _]]
      }
    }

    "seq -> par" should {
      "build a sequential validator" in {
        Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(
            rule(_.zero == 0, NonZeroInt("zero"))
          )
          .par(
            rule(_.negative < 0, NonNegativeInt("negative")),
            rule(_.positive > 0, NonPositiveInt("positive"))
          ) shouldBe a[SeqValidator[_, _]]
      }
    }

    "par -> seq" should {
      "build a sequential validator" in {
        Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .par(
            rule(_.negative < 0, NonNegativeInt("negative")),
            rule(_.positive > 0, NonPositiveInt("positive"))
          )
          .seq(
            rule(_.zero == 0, NonZeroInt("zero"))
          ) shouldBe a[SeqValidator[_, _]]
      }
    }

    "par -> par" should {
      "build a sequential validator" in {
        Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .par(
            rule(_.negative < 0, NonNegativeInt("negative")),
            rule(_.positive > 0, NonPositiveInt("positive"))
          )
          .par(
            rule(_.zero == 0, NonZeroInt("zero")),
            rule(_.nonEmpty.nonEmpty, EmptyString("nonEmpty"))
          ) shouldBe a[SeqValidator[_, _]]
      }
    }
  }

  "SeqValidator" when {
    "created" should {
      "be normalized to the same validator regardless of how it is declared" in {

        val rule1: Rule[FieldError, Data] = rule(_.positive > 0, NonPositiveInt("data.positive"))
        val rule2: Rule[FieldError, Data] = rule(_.negative < 0, NonNegativeInt("data.negative"))
        val rule3: Rule[FieldError, Data] = rule(_.zero == 0, NonZeroInt("data.zero"))

        val normalizedValidator = Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(rule1, rule2, rule3)

        val val1 = Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(rule1)

        val val2 = Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(rule2)

        val val3 = Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(rule3)

        val val12 = Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(rule1, rule2)

        val val23 = Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(rule2, rule3)

        val unnormalizedValidator =
          List(
            Validator.of[Data].withErrorTypeOf[FieldError].seq(normalizedValidator),

            Validator.of[Data].withErrorTypeOf[FieldError].seq(rule1, rule2, rule3),
            Validator.of[Data].withErrorTypeOf[FieldError].seq(rule1, rule2).seq(rule3),
            Validator.of[Data].withErrorTypeOf[FieldError].seq(rule1).seq(rule2, rule3),
            Validator.of[Data].withErrorTypeOf[FieldError].seq(rule1).seq(rule2).seq(rule3),

            Validator.of[Data].withErrorTypeOf[FieldError].seq(val1, val2, val3),
            Validator.of[Data].withErrorTypeOf[FieldError].seq(val1, val2).seq(val3),
            Validator.of[Data].withErrorTypeOf[FieldError].seq(val1).seq(val2, val3),
            Validator.of[Data].withErrorTypeOf[FieldError].seq(val1).seq(val2).seq(val3),

            Validator.of[Data].withErrorTypeOf[FieldError].seq(val12, rule3),
            Validator.of[Data].withErrorTypeOf[FieldError].seq(val12, val3),
            Validator.of[Data].withErrorTypeOf[FieldError].seq(rule1, val23),
            Validator.of[Data].withErrorTypeOf[FieldError].seq(val1, val23)
          )

        forAll(unnormalizedValidator)(_ should === (normalizedValidator))
      }
    }
  }
}
