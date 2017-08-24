/*
 * Problems.scala
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
package patching

import collection.immutable.SortedSet

/**
 * Base class for problems that occur while patching differences or changes.
 */
sealed trait Problems extends Problem

/**
 * Concrete problem implementations.
 */
object Problems extends Problem.Factory[Problems] {

  /**
   * Problem returned when hashes that are expected to match do not.
   *
   * @param expected The value of the expected hash.
   * @param found    The value of the hash that was encountered.
   * @param context  The context where the problem occurred.
   */
  case class HashMismatch(expected: Hash, found: Hash)(implicit val context: Context) extends Problems

  /**
   * Problem returned when keys in a table have no matching keys in a modify.
   *
   * @param missing The keys in the table that did not have matching keys in a modify.
   * @param context The context where the problem occurred.
   */
  case class MissingChangeKeys(missing: SortedSet[Value])(implicit val context: Context) extends Problems

  /**
   * Problem returned when a key in a modify has no matching key in a table.
   *
   * @param expected The key in the modify that did not have a matching key in a table.
   * @param context  The context where the problem occurred.
   */
  case class MissingEntry(expected: Value)(implicit val context: Context) extends Problems

  /**
   * Problem returned when a key in a table was not expected to be there.
   *
   * @param found   The key in the table that was not expected to be there.
   * @param context The context where the problem occurred.
   */
  case class UnexpectedEntry(found: Value)(implicit val context: Context) extends Problems

  /**
   * Problem returned when a fragment expected to be a table is a value instead.
   *
   * @param found   The value that was found where a table was expected.
   * @param context The context where the problem occurred.
   */
  case class TypeMismatch(found: Value)(implicit val context: Context) extends Problems

}
