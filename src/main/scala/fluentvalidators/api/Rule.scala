package com.mucciolo
package fluentvalidators.api

import fluentvalidators.api.impl.SingletonValidator

trait Rule[+E, -A] extends Validator[E, A] {
  override def dimap[B, F](f: B => A, g: E => F): Rule[F, B]
}
