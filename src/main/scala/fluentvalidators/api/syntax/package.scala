package com.mucciolo
package fluentvalidators.api

import fluentvalidators.api.impl.SingletonValidator

package object syntax {
  def rule[E, A](predicate: A => Boolean, caseFalse: E): Rule[E, A] =
    SingletonValidator(predicate, caseFalse)
}
