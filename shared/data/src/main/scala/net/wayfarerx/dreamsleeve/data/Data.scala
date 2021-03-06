/*
 * Data.scala
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

import cats.Eval

/**
 * Base type for data elements.
 */
abstract class Data private[data] {

  /** The cached hash of this element. */
  private var _hash: Option[Hash] = None

  /* Use the equality operation for comparisons. */
  final override def equals(that: Any): Boolean =
    calculateEquals(that).value

  /* Derive the hash code from a prefix of the data hash. */
  final override def hashCode(): Int = {
    val h = Hash.getInternalRepresentation(hash)
    (h(0) & 0x000000FF) << 24 | (h(1) & 0x000000FF) << 16 | (h(2) & 0x000000FF) << 8 | h(3) & 0x000000FF
  }

  /* Use the stringify operation for to string. */
  final override def toString: String =
    calculateToString().value

  /**
   * Returns the hash for this element, generating it if necessary.
   *
   * @return The hash for this element.
   */
  final def hash: Hash =
    _hash getOrElse evalHash(Hash.Generator()).value

  /**
   * Calculate the equality operation for this data element.
   *
   * @param that The instance to test against.
   * @return The equality operation for this data element.
   */
  protected[data] def calculateEquals(that: Any): Eval[Boolean]

  /**
   * Calculate the stringify operation for this data element.
   *
   * @return The stringify operation for this data element.
   */
  protected[data] def calculateToString(): Eval[String]

  /**
   * Calculates the hash for this data.
   *
   * @param generator The hash generator to use.
   * @return The hash for this data.
   */
  protected def calculateHash(generator: Hash.Generator): Eval[Hash]

  /**
   * Creates an operation that can calculate the hash of this element.
   *
   * @param generator The hash generator to use.
   * @return An operation that can calculate the hash of this element.
   */
  final private[data] def evalHash(generator: Hash.Generator): Eval[Hash] =
    _hash map Eval.now getOrElse {
      calculateHash(generator) map { hash =>
        _hash = Some(hash)
        hash
      }
    }

}
