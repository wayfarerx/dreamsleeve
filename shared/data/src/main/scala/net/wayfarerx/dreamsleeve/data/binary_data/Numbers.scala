/*
 * Numbers.scala
 *
 * Copyright 2017 wayfarerx <x@wayfarerx.net> (@thewayfarerx)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.wayfarerx.dreamsleeve.data
package binary_data

import scodec.Codec
import scodec.codecs._

/**
 * Binary support for the number factory object.
 */
trait Numbers {

  /** The implicit fragment discriminator for number values. */
  @inline
  final implicit def binaryAsFragment: Discriminator[Fragment, Value.Number, Int] = Numbers.AsFragment

  /** The implicit value discriminator for number values. */
  @inline
  final implicit def binaryAsValue: Discriminator[Value, Value.Number, Int] = Numbers.AsValue

  /** The implicit number codec. */
  @inline
  final implicit def binaryCodec: Codec[Value.Number] = Numbers.Codec

}

/**
 * Support for binary number codecs.
 */
object Numbers {

  /** The fragment discriminator for number values. */
  val AsFragment: Discriminator[Fragment, Value.Number, Int] = Discriminator(1)

  /** The value discriminator for number values. */
  val AsValue: Discriminator[Value, Value.Number, Int] = Discriminator(AsFragment.value)

  /** The transformation of a small positive whole number into a number. */
  private val fromSmallPositiveWholeNumber: (Int => Value.Number) = Value.Number(_)

  /** The transformation of a number into a small positive whole number. */
  private val toSmallPositiveWholeNumber: PartialFunction[Value.Number, Int] = {
    case n if n.value >= 0 && n.value == n.value.floor && n.value <= Int.MaxValue => n.value.toInt
  }

  /** The transformation of a small fractional number into a number. */
  private val fromSmallFractionalNumber: (Float => Value.Number) = Value.Number(_)

  /** The transformation of a number into a small fractional number. */
  private val toSmallFractionalNumber: PartialFunction[Value.Number, Float] = {
    case n if n.value == n.value.toFloat.toDouble => n.value.toFloat
  }

  /** The number codec. */
  val Codec: Codec[Value.Number] = discriminated[Value.Number].by(uint2)
    .|(0)(toSmallPositiveWholeNumber)(fromSmallPositiveWholeNumber)(vint)
    .|(1)(toSmallFractionalNumber)(fromSmallFractionalNumber)(float)
    .?(2)(Value.Number.unapply)(Value.Number.apply)(double)

}
