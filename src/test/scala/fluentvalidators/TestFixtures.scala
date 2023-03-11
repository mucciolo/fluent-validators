package com.mucciolo
package fluentvalidators

object TestFixtures {

  class Data(val negative: Int, val zero: Int = 0, val positive: Int, val nonEmpty: String = "*")

  sealed trait FieldError {
    def field: String
  }
  object FieldError {
    final case class NonNegativeInt(field: String) extends FieldError
    final case class NonZeroInt(field: String) extends FieldError
    final case class NonPositiveInt(field: String) extends FieldError
    final case class EmptyString(field: String) extends FieldError
  }

  sealed trait SimpleError
  object SimpleError {
    object EmptyString extends SimpleError
    final case class TooLong(max: Int) extends SimpleError
    object InvalidChars extends SimpleError
    object NotEven extends SimpleError
    final case class GreaterThan(upperBound: Int) extends SimpleError
    object NegativeInt extends SimpleError
  }

  final case class DataExt(
    override val negative: Int,
    override val zero: Int,
    override val positive: Int,
    override val nonEmpty: String,
    notFalse: Boolean
  ) extends Data(negative, zero, positive, nonEmpty)

  sealed trait FieldErrorExt extends FieldError
  object FieldErrorExt {
    final case class FalseBoolean(field: String) extends FieldErrorExt
  }

}
