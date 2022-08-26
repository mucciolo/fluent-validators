package com.mucciolo
package fluentvalidators

import fluentvalidators.matchers.ValidatedNecMatchers
import fluentvalidators.ValidatorSpec.*
import fluentvalidators.ValidatorSpec.Error.*
import fluentvalidators.api.Validator
import fluentvalidators.api.Validator.Rule
import fluentvalidators.syntax.ValidatorRuleSyntaxFor

import cats.data.Validated
import cats.implicits.*
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import org.scalatest.prop.*

object ValidatorSpec {

  sealed trait Error
  object Error {
    case object EmptyString extends Error
    case object FalseBoolean extends Error
    case object NonZeroInt extends Error
  }

  case class Data(nonEmpty: String = "something", nonFalse: Boolean, zero: Int)
}

final class ValidatorSpec extends AnyFlatSpec
  with should.Matchers with ValidatedNecMatchers with ValidatorRuleSyntaxFor[Error, Data] {

  private val ruleOne: Rule[Error, Data] = rule(_.nonFalse, FalseBoolean)
  private val ruleTwo: Rule[Error, Data] = rule(_.zero == 0, NonZeroInt)

  private val seqValidator: Validator[Error, Data] =
    Validator.of[Data]
      .withErrorTypeOf[Error]
      .seq(ruleOne, ruleTwo)

  private val parValidator: Validator[Error, Data] =
    Validator.of[Data]
      .withErrorTypeOf[Error]
      .par(ruleOne, ruleTwo)

  private val validatorChain: Validator[Error, Data] =
    Validator.of[Data]
      .withErrorTypeOf[Error]
      .seq(rule(_.nonEmpty.nonEmpty, EmptyString))
      .par(ruleOne, ruleTwo)

  "A sequential validator" should "short-circuit" in {
    val data = Data(nonFalse = false, zero = 1)
    seqValidator.validate(data) should beInvalidDue(FalseBoolean)
  }

  it should "test the next rule of a satisfied rule" in {
    val data = Data(nonFalse = true, zero = 1)
    seqValidator.validate(data) should beInvalidDue(NonZeroInt)
  }

  it should "return the validated object when all rules are satisfied" in {
    val validData = Data(nonFalse = true, zero = 0)
    seqValidator.validate(validData) should beValid(validData)
  }

  "A parallel validator" should "apply rules parallelly" in {
    val data = Data(nonFalse = false, zero = 1)
    parValidator.validate(data) should beInvalidDue(FalseBoolean, NonZeroInt)
  }

  it should "apply rules independently" in {

    val dataNotFalse = Data(nonFalse = false, zero = 0)
    parValidator.validate(dataNotFalse) should beInvalidDue(FalseBoolean)

    val dataNonZero = Data(nonFalse = true, zero = 1)
    parValidator.validate(dataNonZero) should beInvalidDue(NonZeroInt)

  }

  it should "return the validated object when all rules are satisfied" in {
    val validData = Data(nonFalse = true, zero = 0)
    parValidator.validate(validData) should beValid(validData)
  }

  "A validation chain" should "short-circuit" in {
    val data = Data(nonEmpty = "", nonFalse = false, zero = 1)
    validatorChain.validate(data) should beInvalidDue(EmptyString)
  }

  it should "apply the next validator of a satisfied validator" in {
    val data = Data(nonEmpty = "something", nonFalse = false, zero = 1)
    validatorChain.validate(data) should beInvalidDue(FalseBoolean, NonZeroInt)
  }

}
