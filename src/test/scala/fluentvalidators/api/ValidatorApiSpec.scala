package com.mucciolo
package fluentvalidators.api

import fluentvalidators.TestFixtures.*
import fluentvalidators.TestFixtures.Error.*
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

final class ValidatorApiSpec extends AnyWordSpec with should.Matchers {

  "Validator" when {

    "just created" should {
      "be empty" in {
        Validator.of[Data].withErrorTypeOf[Error] shouldBe an[EmptyValidator[Error, Data]]
      }
    }

    "seq" should {
      "build a sequential validator" in {
        Validator.of[Data]
          .withErrorTypeOf[Error]
          .seq(rule(_.zero == 0, NonZeroInt("zero"))) shouldBe a[SeqValidator[Error, Data]]
      }
    }

    "par" should {
      "build a parallel validator" in {
        Validator.of[Data]
          .withErrorTypeOf[Error]
          .par(
            rule(_.negative < 0, NonNegativeInt("negative")),
            rule(_.positive > 0, NonPositiveInt("positive"))
          ) shouldBe a[ParValidator[Error, Data]]
      }
    }

    "seq -> par" should {
      "build a sequential validator" in {
        Validator.of[Data]
          .withErrorTypeOf[Error]
          .seq(
            rule(_.zero == 0, NonZeroInt("zero"))
          )
          .par(
            rule(_.negative < 0, NonNegativeInt("negative")),
            rule(_.positive > 0, NonPositiveInt("positive"))
          ) shouldBe a[SeqValidator[Error, Data]]
      }
    }

    "par -> seq" should {
      "build a sequential validator" in {
        Validator.of[Data]
          .withErrorTypeOf[Error]
          .par(
            rule(_.negative < 0, NonNegativeInt("negative")),
            rule(_.positive > 0, NonPositiveInt("positive"))
          )
          .seq(
            rule(_.zero == 0, NonZeroInt("zero"))
          ) shouldBe a[SeqValidator[Error, Data]]
      }
    }

    "par -> par" should {
      "build a sequential validator" in {
        Validator.of[Data]
          .withErrorTypeOf[Error]
          .par(
            rule(_.negative < 0, NonNegativeInt("negative")),
            rule(_.positive > 0, NonPositiveInt("positive"))
          )
          .par(
            rule(_.zero == 0, NonZeroInt("zero")),
            rule(_.nonEmpty.nonEmpty, EmptyString("nonEmpty"))
          ) shouldBe a[SeqValidator[Error, Data]]
      }
    }
  }
}
