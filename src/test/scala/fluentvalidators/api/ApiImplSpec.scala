package com.mucciolo
package fluentvalidators.api

import fluentvalidators.TestFixtures.*
import fluentvalidators.TestFixtures.FieldError.*
import fluentvalidators.api.Validator
import fluentvalidators.api.impl.*
import fluentvalidators.api.syntax.*
import fluentvalidators.matchers.ValidatedNecMatchers

import org.scalatest.*
import org.scalatest.Inspectors.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import org.scalatest.prop.*
import org.scalatest.wordspec.AnyWordSpec

final class ApiImplSpec extends AnyWordSpec with should.Matchers with ValidatedNecMatchers {

  sealed trait StringError
  case object EmptyString extends StringError
  case object ContainsSpecialChars extends StringError

  sealed trait IntError
  case object ZeroOrNegativeInt extends IntError
  case object NotEvenInt extends IntError

  final case class User(name: String)
  sealed trait UserError
  case object EmptyName extends UserError
  case object InvalidName extends UserError

  "Validator" when {

    "just created" should {
      "be empty" in {
        Validator.of[Data].withErrorTypeOf[FieldError] shouldBe an[EmptyValidator[_, _]]
      }
    }

    "seq" should {
      "build a singleton validator given a single rule" in {
        Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(rule(_.zero == 0, NonZeroInt("zero"))) shouldBe a[SingletonValidator[_, _]]
      }

      "build a sequential validator given two rules" in {
        Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(
            rule(_.negative < 0, NonNegativeInt("negative")),
            rule(_.positive > 0, NonPositiveInt("positive"))
          ) shouldBe a[SeqValidator[_, _]]
      }
    }

    "par" should {
      "build a parallel validator" in {
        Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .par(
            rule(_.negative < 0, NonNegativeInt("negative")),
            rule(_.positive > 0, NonPositiveInt("positive"))
          ) shouldBe a[ParValidator[_, _]]
      }
    }

    "seq -> par" should {
      "build a sequential validator" in {
        Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(
            rule(_.zero == 0, NonZeroInt("zero"))
          )
          .par(
            rule(_.negative < 0, NonNegativeInt("negative")),
            rule(_.positive > 0, NonPositiveInt("positive"))
          ) shouldBe a[SeqValidator[_, _]]
      }
    }

    "par -> seq" should {
      "build a sequential validator" in {
        Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .par(
            rule(_.negative < 0, NonNegativeInt("negative")),
            rule(_.positive > 0, NonPositiveInt("positive"))
          )
          .seq(
            rule(_.zero == 0, NonZeroInt("zero"))
          ) shouldBe a[SeqValidator[_, _]]
      }
    }

    "par -> par" should {
      "build a sequential validator" in {
        Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .par(
            rule(_.negative < 0, NonNegativeInt("negative")),
            rule(_.positive > 0, NonPositiveInt("positive"))
          )
          .par(
            rule(_.zero == 0, NonZeroInt("zero")),
            rule(_.nonEmpty.nonEmpty, FieldError.EmptyString("nonEmpty"))
          ) shouldBe a[SeqValidator[_, _]]
      }
    }
  }

  "EmptyValidator" when {

    val stringEmptyValidator = EmptyValidator[StringError, String]()

    "dimap" should {

      val userEmptyValidator: EmptyValidator[UserError, User] =
        stringEmptyValidator.dimap(
          user => user.name,
          {
            case EmptyString => EmptyName
            case ContainsSpecialChars => InvalidName
          }
        )

      "create a new empty validator with mapping types" in {
        userEmptyValidator shouldBe a[EmptyValidator[_, _]]
      }
    }

  }

  "SingletonValidator" when {

    val stringSingletonValidator = SingletonValidator[StringError, String](_.nonEmpty, EmptyString)

    "dimap" should {

      val userSingletonValidator: SingletonValidator[UserError, User] =
        stringSingletonValidator.dimap(
          user => user.name,
          {
            case EmptyString => EmptyName
            case ContainsSpecialChars => InvalidName
          }
        )

      "create a new singleton validator" in {
        userSingletonValidator shouldBe a[SingletonValidator[_, _]]
      }

      "project the new validator onto the mapped validator" in {
        stringSingletonValidator.validate("") should beInvalidDue(EmptyString)
        userSingletonValidator.validate(User(name = "")) should beInvalidDue(EmptyName)

        stringSingletonValidator.validate("nonempty") should beValid("nonempty")
        userSingletonValidator.validate(User(name = "nonempty")) should beValid(User(name = "nonempty"))
      }
    }

  }

  "SeqValidator" when {
    "created" should {
      "be normalized to the same validator regardless of how it is declared" in {

        val rule1: Rule[FieldError, Data] = rule(_.positive > 0, NonPositiveInt("data.positive"))
        val rule2: Rule[FieldError, Data] = rule(_.negative < 0, NonNegativeInt("data.negative"))
        val rule3: Rule[FieldError, Data] = rule(_.zero == 0, NonZeroInt("data.zero"))

        val normalizedValidator = Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(rule1, rule2, rule3)

        val val1 = Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(rule1)

        val val2 = Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(rule2)

        val val3 = Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(rule3)

        val val12 = Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(rule1, rule2)

        val val23 = Validator.of[Data]
          .withErrorTypeOf[FieldError]
          .seq(rule2, rule3)

        val unnormalizedValidator =
          List(
            Validator.of[Data].withErrorTypeOf[FieldError].seq(normalizedValidator),

            Validator.of[Data].withErrorTypeOf[FieldError].seq(rule1, rule2, rule3),
            Validator.of[Data].withErrorTypeOf[FieldError].seq(rule1, rule2).seq(rule3),
            Validator.of[Data].withErrorTypeOf[FieldError].seq(rule1).seq(rule2, rule3),
            Validator.of[Data].withErrorTypeOf[FieldError].seq(rule1).seq(rule2).seq(rule3),

            Validator.of[Data].withErrorTypeOf[FieldError].seq(val1, val2, val3),
            Validator.of[Data].withErrorTypeOf[FieldError].seq(val1, val2).seq(val3),
            Validator.of[Data].withErrorTypeOf[FieldError].seq(val1).seq(val2, val3),
            Validator.of[Data].withErrorTypeOf[FieldError].seq(val1).seq(val2).seq(val3),

            Validator.of[Data].withErrorTypeOf[FieldError].seq(val12, rule3),
            Validator.of[Data].withErrorTypeOf[FieldError].seq(val12, val3),
            Validator.of[Data].withErrorTypeOf[FieldError].seq(rule1, val23),
            Validator.of[Data].withErrorTypeOf[FieldError].seq(val1, val23)
          )

        forAll(unnormalizedValidator)(_ should ===(normalizedValidator))
      }
    }

    val stringSeqValidator = SeqValidator[StringError, String](
      SingletonValidator(_.nonEmpty, EmptyString),
      SingletonValidator(_.forall(c => c.isLetter || c.isSpaceChar), ContainsSpecialChars)
    )

    "dimap" should {

      val userSeqValidator: SeqValidator[UserError, User] =
        stringSeqValidator.dimap(
          user => user.name,
          {
            case EmptyString => EmptyName
            case ContainsSpecialChars => InvalidName
          }
        )

      "create a new sequential validator with mapping types" in {
        userSeqValidator shouldBe a[SeqValidator[_, _]]
      }

      "project the new validator onto the mapped validator" in {
        stringSeqValidator.validate("") should beInvalidDue(EmptyString)
        userSeqValidator.validate(User(name = "")) should beInvalidDue(EmptyName)

        stringSeqValidator.validate("Test_") should beInvalidDue(ContainsSpecialChars)
        userSeqValidator.validate(User(name = "Test_")) should beInvalidDue(InvalidName)

        stringSeqValidator.validate("Test") should beValid("Test")
        userSeqValidator.validate(User(name = "Test")) should beValid(User(name = "Test"))
      }

    }

  }

  "ParValidator" when {

    val intParValidator = ParValidator[IntError, Int](
      SingletonValidator[IntError, Int](_ > 0, ZeroOrNegativeInt),
      SingletonValidator[IntError, Int](_ % 2 == 0, NotEvenInt)
    )

    "dimap" should {

      final case class PositiveEvenInt(n: Int)
      sealed trait PositiveEvenIntError
      case object NonPositiveInt extends PositiveEvenIntError
      case object OddInt extends PositiveEvenIntError

      val wrappedPosEvenIntParValidator: ParValidator[PositiveEvenIntError, PositiveEvenInt] =
        intParValidator.dimap(
          posEvenInt => posEvenInt.n,
          {
            case ZeroOrNegativeInt => NonPositiveInt
            case NotEvenInt => OddInt
          }
        )

      "create a new parallel validator" in {
        wrappedPosEvenIntParValidator shouldBe a[ParValidator[_, _]]
      }

      "project the new validator onto the mapped validator" in {
        intParValidator.validate(-1) should beInvalidDue(ZeroOrNegativeInt, NotEvenInt)
        wrappedPosEvenIntParValidator.validate(PositiveEvenInt(-1)) should beInvalidDue(NonPositiveInt, OddInt)

        intParValidator.validate(0) should beInvalidDue(ZeroOrNegativeInt)
        wrappedPosEvenIntParValidator.validate(PositiveEvenInt(0)) should beInvalidDue(NonPositiveInt)

        intParValidator.validate(1) should beInvalidDue(NotEvenInt)
        wrappedPosEvenIntParValidator.validate(PositiveEvenInt(1)) should beInvalidDue(OddInt)

        intParValidator.validate(2) should beValid(2)
        wrappedPosEvenIntParValidator.validate(PositiveEvenInt(2)) should beValid(PositiveEvenInt(2))
      }
    }
  }
}
