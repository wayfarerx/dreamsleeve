/*
 * Modifies.scala
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

import language.implicitConversions

import cats.data._
import cats.implicits._
import Validated.{invalid, valid}

import Problems._

/**
 * Support for the modification factory object.
 */
trait Modifies {

  /**
   * Wraps any modification with the patching interface.
   *
   * @param modify The modification to wrap.
   * @return The patching interface.
   */
  @inline
  implicit def modifyToModifyPatch(modify: Update.Modify): Modifies.ModifyPatch =
  new Modifies.ModifyPatch(modify)

}

/**
 * Definitions associated with modify patching.
 */
object Modifies {

  /**
   * The patching interface for modifications.
   *
   * @param modify The modification to provide the patching interface for.
   */
  final class ModifyPatch(val modify: Update.Modify) extends AnyVal {

    /**
     * Applies the update by verifying the original table, applying all changes and returning the resulting table.
     *
     * @param fromFragment The fragment that is being modified.
     * @param hasher       The hasher to generate hashes with.
     * @return The resulting table or problems that were encountered modifying the fragment.
     */
    def patch(fromFragment: Fragment)(implicit hasher: Hasher): Result[Fragment] =
      patching(fromFragment)(hasher)

    /**
     * Applies the update by verifying the original table, applying all changes and returning the resulting table.
     *
     * @param fromFragment The fragment that is being modified.
     * @param h            The hasher to generate hashes with.
     * @return The resulting table or problems that were encountered modifying the fragment.
     */
    private[patching] def patching(fromFragment: Fragment)(implicit h: Hasher): Attempt[Fragment] = {
      // Verify that the from table's hash matches.
      val hashCheck: Attempt[Vector[(Value, Fragment)]] =
        if (modify.fromHash == fromFragment.hash) valid(Vector())
        else invalid(Problems.List.of(HashMismatch(modify.fromHash, fromFragment.hash)))
      (fromFragment match {
        case v@Value() =>
          NonEmptyList.of(hashCheck, invalid(Problems.List.of(TypeMismatch(v)))).reduce
        case fromTable@Table(_) =>
          // Verify that all from keys have matching changes.
          val keyCheck = if ((fromTable.keys -- modify.changes.keys).isEmpty) valid(Vector()) else
            invalid(Problems.List.of(MissingChangeKeys(fromTable.keys -- modify.changes.keys)))
          // Apply all the changes.
          val entries = modify.changes map { case (k, v) =>
            v match {
              case Change.Add(_) if fromTable.get(k).nonEmpty => invalid(Problems.List.of(UnexpectedEntry(k)))
              case add@Change.Add(_) => add.patching() map (v => Vector(k -> v))
              case _ if fromTable.get(k).isEmpty => invalid(Problems.List.of(MissingEntry(k)))
              case remove@Change.Remove(_) => remove.patching(fromTable(k)) map (_ => Vector())
              case update@Update() => update.patching(fromTable(k)) map (v => Vector(k -> v))
            }
          }
          // Construct the resulting table.
          NonEmptyList(hashCheck, keyCheck :: entries.toList).reduce
      }) map (e => Table(e: _*))
    }

  }

}
