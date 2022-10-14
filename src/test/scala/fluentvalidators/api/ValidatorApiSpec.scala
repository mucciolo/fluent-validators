package com.mucciolo
package fluentvalidators.api

import fluentvalidators.TestFixtures.*
import fluentvalidators.api.Validator
import fluentvalidators.api.Validator.Rule
import fluentvalidators.api.impl.*
import fluentvalidators.syntax.ValidatorRuleSyntaxFor

import fluentvalidators.TestFixtures.Error.*
import org.scalatest.*
import org.scalatest.Inspectors.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import org.scalatest.prop.*

final class ValidatorApiSpec extends AnyFlatSpec
                             with should.Matchers with ValidatorRuleSyntaxFor[Error, Data] {

  "Validator API" should "create an empty validator" in {
    Validator.of[Data].withErrorTypeOf[Error] shouldBe a [EmptyValidator[Error, Data]]
  }

  it should "create a sequential validator" in {
    Validator.of[Data]
      .withErrorTypeOf[Error]
      .seq(rule(_.zero == 0, NonZeroInt)) shouldBe a [SeqValidator[Error, Data]]
  }

  it should "create a parallel validator" in {
    Validator.of[Data]
      .withErrorTypeOf[Error]
      .par(rule(_.zero == 0, NonZeroInt)) shouldBe a [ParValidator[Error, Data]]
  }

}
