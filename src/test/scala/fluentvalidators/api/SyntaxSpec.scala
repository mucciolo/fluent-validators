package com.mucciolo
package fluentvalidators.api

import fluentvalidators.api.impl.EmptyValidator
import fluentvalidators.api.syntax.*
import fluentvalidators.matchers.ValidatedNecMatchers

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

final class SyntaxSpec extends AnyWordSpec with should.Matchers with ValidatedNecMatchers {

  private class A
  private class E

  "Syntax" when {
    "Validator.of" should {
      "create a validator builder" in {
        val builder: ValidatorBuilder[A] = Validator.of[A]

        builder should === (builder)
      }
    }

    "builder.withErrorTypeOf" should {
      "create an empty validator" in {
        val builder: ValidatorBuilder[A] = new ValidatorBuilder[A]
        val validator: Validator[E, A] = builder.withErrorTypeOf[E]

        validator shouldBe a[EmptyValidator[_, _]]
      }
    }

    "rule" should {
      "create a rule with the given predicate" in {
        case object EmptyString extends E
        val nonEmptyStringRule = rule[E, String](_.nonEmpty, EmptyString)

        nonEmptyStringRule.validate("") should beInvalidDue(EmptyString)
        nonEmptyStringRule.validate("nonempty") should beValid("nonempty")
      }
    }
  }

}
