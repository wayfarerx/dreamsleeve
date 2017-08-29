/*
 * Hashable.scala
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

/**
 * Base type for data elements that can be hashed.
 */
trait Hashable {

  /**
   * Returns the hash for this data element, generating it if necessary.
   *
   * @param hasher The hasher to generate a hash with if necessary.
   * @return The hash for this data element.
   */
  def hash(implicit hasher: Hasher): Hash

}

/**
 * Types associated with data elements that can be hashed.
 */
object Hashable {

  /**
   * Supporting class for hashable data types.
   */
  abstract class Support extends Hashable {

    /** The cached hash of this data element. */
    private var _hash: Option[Hash] = None

    /* Return the hash for this data element, generating it if necessary. */
    final override def hash(implicit hasher: Hasher): Hash =
      _hash getOrElse {
        val hash = generateHash(hasher)
        _hash = Some(hash)
        hash
      }

    /**
     * Generates the hash for this data element.
     *
     * @param hasher The hasher to generate a hash with.
     * @return The hash for this data element.
     */
    protected def generateHash(hasher: Hasher): Hash

  }

}