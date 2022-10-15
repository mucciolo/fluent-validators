package com.mucciolo
package fluentvalidators.syntax

import fluentvalidators.api.Validator
import fluentvalidators.api.Rule

trait ValidatorRuleSyntaxFor[E, A] {
  inline protected def rule(inline predicate: A => Boolean, inline caseFalse: E): Rule[E, A] =
    Rule.rule[E, A](predicate, caseFalse)
}
