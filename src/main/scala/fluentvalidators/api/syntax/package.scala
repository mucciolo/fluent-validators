package com.mucciolo
package fluentvalidators.api

import fluentvalidators.api.impl.{EmptyValidator, SingletonValidator}

package object syntax {

  object Validator {
    inline def of[A] = new ValidatorBuilder[A]()
  }

  final class ValidatorBuilder[-A]() {
    inline def withErrorTypeOf[E]: Validator[E, A] = new EmptyValidator[E, A]()
  }

  def rule[E, A](predicate: A => Boolean, caseFalse: E): Rule[E, A] =
    SingletonValidator(predicate, caseFalse)

}
