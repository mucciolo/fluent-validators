package com.mucciolo
package fluentvalidators.matchers

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import org.scalatest.matchers.{MatchResult, Matcher}

trait ValidatedNecMatchers {

  def beValid[T](element: T): Matcher[Validated[_, T]] = new BeValidMatcher[T](element)

  final private class BeValidMatcher[T](instance: T) extends Matcher[Validated[_, T]] {
    def apply(validated: Validated[_, T]): MatchResult =
      MatchResult(
        validated.fold(_ => false, _ == instance),
        s"$validated was not equal to Valid($instance)",
        s"$validated was not different than Valid($instance)"
      )
  }

  def beInvalidDue[E](headError: E, tailErrors: E*): Matcher[ValidatedNec[E, _]] =
    new BeInvalidMatcher[E](headError, tailErrors: _*)

  final private class BeInvalidMatcher[E](headError: E, tailErrors: E*)
    extends Matcher[ValidatedNec[E, _]] {

    private val errorsNec: NonEmptyChain[E] = NonEmptyChain.of(headError, tailErrors: _*)

    def apply(validated: ValidatedNec[E, _]): MatchResult =
      MatchResult(
        validated.fold(_ == errorsNec, _ => false),
        s"$validated was not equal to Invalid($errorsNec)",
        s"$validated was not different than Invalid($errorsNec)"
      )
  }

}
