package com.mucciolo
package fluentvalidators.syntax

import com.mucciolo.fluentvalidators.api.Validator.Rule
import com.mucciolo.fluentvalidators.api.Validator

trait ValidatorRuleSyntaxFor[E, A] {
  inline protected def rule: (A => Boolean, E) => Rule[E, A] = Validator.rule[E, A](_, _)
}
