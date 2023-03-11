package com.mucciolo
package fluentvalidators.api

import fluentvalidators.TestFixtures.*
import fluentvalidators.TestFixtures.FieldError.*
import fluentvalidators.api.Rule.rule
import fluentvalidators.api.Validator
import fluentvalidators.api.Validator.*
import fluentvalidators.api.impl.*

import org.scalatest.*
import org.scalatest.Inspectors.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import org.scalatest.prop.*
import org.scalatest.wordspec.AnyWordSpec

// TODO test narrow and contramap
final class ValidatorApiSpec extends AnyWordSpec with should.Matchers {

  "Validator" when {

    "just created" should {
      "be empty" in {
        Validator.of[Data].withErrorTypeOf[FieldError] shouldBe an[EmptyValidator[FieldError, Data]]
      }
    }

    "seq" should {
      "build a sequential validator" in {
        Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(rule(_.zero == 0, NonZeroInt("zero"))) shouldBe a[SeqValidator[FieldError, Data]]
      }
    }

    "par" should {
      "build a parallel validator" in {
        Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .par(
            rule(_.negative < 0, NonNegativeInt("negative")),
            rule(_.positive > 0, NonPositiveInt("positive"))
          ) shouldBe a[ParValidator[FieldError, Data]]
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
          ) shouldBe a[SeqValidator[FieldError, Data]]
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
          ) shouldBe a[SeqValidator[FieldError, Data]]
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
          ) shouldBe a[SeqValidator[FieldError, Data]]
      }
    }
  }
}
