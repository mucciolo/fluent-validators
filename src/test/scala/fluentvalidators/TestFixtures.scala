package com.mucciolo
package fluentvalidators

object TestFixtures {

  class Data(val negative: Int, val zero: Int = 0, val positive: Int, val nonEmpty: String = "*")

  sealed trait Error {
    def field: String
  }
  object Error {
    final case class NonNegativeInt(field: String) extends Error
    final case class NonZeroInt(field: String) extends Error
    final case class NonPositiveInt(field: String) extends Error
    final case class EmptyString(field: String) extends Error
  }

  final case class DataExt(
    override val negative: Int,
    override val zero: Int,
    override val positive: Int,
    override val nonEmpty: String,
    notFalse: Boolean
  ) extends Data(negative, zero, positive, nonEmpty)

  sealed trait ErrorExt extends Error
  object ErrorExt {
    final case class FalseBoolean(field: String) extends ErrorExt
  }

}
