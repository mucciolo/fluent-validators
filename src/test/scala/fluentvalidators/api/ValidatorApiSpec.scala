package com.mucciolo
package fluentvalidators.api

import fluentvalidators.TestFixtures.*
import fluentvalidators.api.Validator
import fluentvalidators.api.Validator.*
import fluentvalidators.api.impl.*
import fluentvalidators.syntax.ValidatorRuleSyntaxFor

import fluentvalidators.TestFixtures.Error.*
import org.scalatest.*
import org.scalatest.Inspectors.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import org.scalatest.prop.*

final class ValidatorApiSpec extends AnyFlatSpec
                             with should.Matchers
                             with ValidatorRuleSyntaxFor[Error, Data] {

  "Validator API" should "build an empty validator" in {
    Validator.of[Data].withErrorTypeOf[Error] shouldBe an [EmptyValidator[Error, Data]]
  }

  it should "build a sequential validator of 'seq' call" in {
    Validator.of[Data]
      .withErrorTypeOf[Error]
      .seq(rule(_.zero == 0, NonZeroInt)) shouldBe a [SeqValidator[Error, Data]]
  }

  it should "build a parallel validator of 'par' call" in {
    Validator.of[Data]
      .withErrorTypeOf[Error]
      .par(
        rule(_.negative < 0, NonNegativeInt),
        rule(_.positive > 0, NonPositiveInt)
      ) shouldBe a [ParValidator[Error, Data]]
  }

  it should "build a sequential validator of 'seq' followed by 'par'" in {
    Validator.of[Data]
      .withErrorTypeOf[Error]
      .seq(
        rule(_.zero == 0, NonZeroInt)
      )
      .par(
        rule(_.negative < 0, NonNegativeInt),
        rule(_.positive > 0, NonPositiveInt)
      ) shouldBe a [SeqValidator[Error, Data]]
  }

  it should "build a sequential validator of 'par' followed by 'seq'" in {
    Validator.of[Data]
      .withErrorTypeOf[Error]
      .par(
        rule(_.negative < 0, NonNegativeInt),
        rule(_.positive > 0, NonPositiveInt)
      )
      .seq(
        rule(_.zero == 0, NonZeroInt)
      ) shouldBe a [SeqValidator[Error, Data]]
  }

  it should "build a sequential validator of 'par' followed by 'par'" in {
    Validator.of[Data]
      .withErrorTypeOf[Error]
      .par(
        rule(_.negative < 0, NonNegativeInt),
        rule(_.positive > 0, NonPositiveInt)
      )
      .par(
        rule(_.zero == 0, NonZeroInt),
        rule(_.nonEmpty.nonEmpty, EmptyString)
      ) shouldBe a [SeqValidator[Error, Data]]
  }

}
