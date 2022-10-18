package com.mucciolo
package fluentvalidators

object TestFixtures {

  class Data(val negative: Int, val zero: Int = 0, val positive: Int, val nonEmpty: String = "*")

  sealed trait Error
  object Error {
    case object NonNegativeInt extends Error
    case object NonZeroInt extends Error
    case object NonPositiveInt extends Error
    case object EmptyString extends Error
  }

  final case class DataExt(
    override val negative: Int,
    override val zero    : Int,
    override val positive: Int,
    override val nonEmpty: String,
    notFalse             : Boolean
  ) extends Data(negative, zero, positive, nonEmpty)

  sealed trait ErrorExt extends Error
  object ErrorExt {
    case object FalseBoolean extends ErrorExt
  }

}
