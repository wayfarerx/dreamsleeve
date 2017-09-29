/*
 * Strings.scala
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
 * Binary support for the string factory object.
 */
trait Strings {

  /** The implicit fragment discriminator for string values. */
  @inline
  final implicit def binaryAsFragment: Discriminator[Fragment, Value.String, Int] = Strings.AsFragment

  /** The implicit value discriminator for string values. */
  @inline
  final implicit def binaryAsValue: Discriminator[Value, Value.String, Int] = Strings.AsValue

  /** The implicit string codec. */
  @inline
  final implicit def binaryCodec: Codec[Value.String] = Strings.Codec

}

/**
 * Support for binary string codecs.
 */
object Strings {

  /** The fragment discriminator for string values. */
  val AsFragment: Discriminator[Fragment, Value.String, Int] = Discriminator(2)

  /** The value discriminator for string values. */
  val AsValue: Discriminator[Value, Value.String, Int] = Discriminator(AsFragment.value)

  /** The string codec. */
  val Codec: Codec[Value.String] = variableSizeBytes(uint16, utf8).as[Value.String]

}
