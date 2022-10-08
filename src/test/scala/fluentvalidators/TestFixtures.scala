package com.mucciolo
package fluentvalidators

object TestFixtures {

  case class Data(negative: Int, zero: Int = 0, positive: Int)

  sealed trait Error
  object Error {
    case object NonNegativeInt extends Error
    case object NonZeroInt extends Error
    case object NonPositiveInt extends Error
  }

}
