package com.mucciolo
package fluentvalidators.api

import fluentvalidators.TestFixtures.*
import fluentvalidators.api.Validator
import fluentvalidators.api.Validator.Rule
import fluentvalidators.api.impl.*
import fluentvalidators.syntax.ValidatorRuleSyntaxFor

import org.scalatest.*
import org.scalatest.Inspectors.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import org.scalatest.prop.*

final class ValidatorApiSpec extends AnyFlatSpec
  with should.Matchers with ValidatorRuleSyntaxFor[Error, Data] {

  "Validator API" should "create empty validator" in {
    Validator.of[Data].withErrorTypeOf[Error] shouldBe a [EmptyValidator[Error, Data]]
  }

}
