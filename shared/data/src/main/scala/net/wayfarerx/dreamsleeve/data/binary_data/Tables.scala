/*
 * Tables.scala
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
 * Binary support for the table factory object.
 */
trait Tables {

  /** The implicit fragment discriminator for tables. */
  @inline
  final implicit def binaryAsFragment: Discriminator[Fragment, Table, Int] = Tables.AsFragment

  /** The implicit table codec. */
  @inline
  final implicit def binaryCodec: Codec[Table] = Tables.Codec

}

/**
 * Support for binary table codecs.
 */
object Tables {

  /** The fragment discriminator for tables. */
  val AsFragment: Discriminator[Fragment, Table, Int] = Discriminator(3)

  /** The table codec. */
  val Codec: Codec[Table] =
    lazily(vectorOfN(uint16, Values.Codec ~ Fragments.Codec).xmap(Table(_: _*), _.entries.toVector))

}