package com.mucciolo
package fluentvalidators.api

import fluentvalidators.api.Validator
import fluentvalidators.api.syntax.*

import cats.data.ValidatedNec
import cats.implicits.catsSyntaxValidatedIdBinCompat0
import com.mucciolo.fluentvalidators.matchers.ValidatedNecMatchers
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

final class ApiTypeSpec extends AnyWordSpec with should.Matchers with ValidatedNecMatchers {

  "Validator" when {
    "validatee type is A and B <: A" should {

      class A
      class B extends A
      val validatorA: Validator[Any, A] = Validator.of[A].withErrorTypeOf[Any]

      "be a validator of type B" in {
        val validatorB: Validator[Any, B] = validatorA

        validatorA should === (validatorB)
      }

      "preserve validated subclass type" in {
        val b = new B
        val validB: ValidatedNec[Any, B] = validatorA.validate(b)

        validB should beValid(b)
      }

    }

    "error type is F and F <: E" should {

      class E
      class F extends E

      "be a validator with error type E" in {
        val f: Validator[F, Any] = Validator.of[Any].withErrorTypeOf[F]
        val e: Validator[E, Any] = f

        f should === (e)
      }
    }
  }

}