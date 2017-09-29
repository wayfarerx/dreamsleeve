/*
 * Booleans.scala
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
 * Binary support for the boolean factory object.
 */
trait Booleans {

  /** The implicit fragment discriminator for boolean values. */
  @inline
  final implicit def binaryAsFragment: Discriminator[Fragment, Value.Boolean, Int] = Booleans.AsFragment

  /** The implicit value discriminator for boolean values. */
  @inline
  final implicit def binaryAsValue: Discriminator[Value, Value.Boolean, Int] = Booleans.AsValue

  /** The implicit boolean codec. */
  @inline
  final implicit def binaryCodec: Codec[Value.Boolean] = Booleans.Codec

}

/**
 * Support for binary boolean codecs.
 */
object Booleans {

  /** The fragment discriminator for boolean values. */
  val AsFragment: Discriminator[Fragment, Value.Boolean, Int] = Discriminator(0)

  /** The value discriminator for boolean values. */
  val AsValue: Discriminator[Value, Value.Boolean, Int] = Discriminator(AsFragment.value)

  /** The boolean codec. */
  val Codec: Codec[Value.Boolean] = bool.as[Value.Boolean]

}
