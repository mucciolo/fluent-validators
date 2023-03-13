package com.mucciolo
package fluentvalidators.api

import fluentvalidators.api.impl.{EmptyValidator, SingletonValidator}

import cats.data.ValidatedNec

package object syntax {

  object Validator {
    inline def of[A] = new ValidatorBuilder[A]()
  }

  final class ValidatorBuilder[-A]() {
    inline def withErrorTypeOf[E]: Validator[E, A] = new EmptyValidator[E, A]()
  }

  def rule[E, A](predicate: A => Boolean, caseFalse: E): Rule[E, A] =
    SingletonValidator[E, A](predicate, caseFalse)

  inline def validate[E, A](inline instance: A)(using validator: Validator[E, A]): ValidatedNec[E, A] =
    validator.validate(instance)

  extension [A](instance: A) {
    inline def validated[E](using Validator[E, A]): ValidatedNec[E, A] = validate(instance)
  }

}
