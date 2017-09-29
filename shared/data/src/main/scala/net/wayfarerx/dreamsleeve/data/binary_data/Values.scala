/*
 * Values.scala
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
 * Binary support for the value factory object.
 */
trait Values {

  /** The implicit marker denoting that value is discriminated. */
  @inline
  final implicit def binaryDiscriminated: Discriminated[Value, Int] = Values.Discriminated

  /** The implicit value codec. */
  @inline
  final implicit def binaryCodec: Codec[Value] = Values.Codec

}

/**
 * Support for binary value codecs.
 */
object Values {

  /** The marker denoting that value is discriminated. */
  val Discriminated: Discriminated[Value, Int] = scodec.codecs.Discriminated(uint2)

  /** The value codec. */
  val Codec: Codec[Value] = (Booleans.Codec :+: Numbers.Codec :+: Strings.Codec).as[Value].auto

}