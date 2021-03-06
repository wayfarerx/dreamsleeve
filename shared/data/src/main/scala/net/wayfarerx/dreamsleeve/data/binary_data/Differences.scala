/*
 * Differences.scala
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
 * Binary support for the difference factory object.
 */
trait Differences {

  /** The marker denoting that difference is discriminated. */
  @inline
  final implicit def binaryDiscriminated: Discriminated[Difference, Int] = Differences.Discriminated

  /** The implicit difference codec. */
  @inline
  final implicit def binaryCodec: Codec[Difference] = Differences.Codec

}

/**
 * Support for binary difference codecs.
 */
object Differences {

  /** The marker denoting that difference is discriminated. */
  val Discriminated: Discriminated[Difference, Int] = scodec.codecs.Discriminated(uint(2))

  /** The difference codec. */
  val Codec: Codec[Difference] =
    (Creates.Codec :+: Revises.Codec :+: Deletes.Codec).as[Difference].auto

}
