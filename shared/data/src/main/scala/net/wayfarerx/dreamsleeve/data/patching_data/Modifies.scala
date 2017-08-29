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
package patching_data

import language.implicitConversions

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
  implicit def modifyToModifiesPatch(modify: Update.Modify): Modifies.ModifyPatch =
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
     * @return The resulting table or any problem that was encountered modifying the fragment.
     */
    def patch(fromFragment: Fragment)(implicit hasher: Hasher): Either[Problems, Fragment] =
    // Verify that the from table's hash matches.
      if (modify.fromHash != fromFragment.hash) Left(Problems.HashMismatch(modify.fromHash, fromFragment.hash))
      else fromFragment match {
        case v@Value() =>
          Left(Problems.TypeMismatch(v))
        case fromTable@Table(_) =>
          // Verify that all from keys have matching changes.
          val missingKeys = fromTable.keys -- modify.changes.keys
          if (missingKeys.nonEmpty) {
            Left(Problems.MissingChangeKeys(missingKeys))
          } else {
            // Apply all the changes.
            val empty: Either[Problems, Vector[(Value, Fragment)]] = Right(Vector[(Value, Fragment)]())
            (empty /: modify.changes) { (e: Either[Problems, Vector[(Value, Fragment)]], c: (Value, Change)) =>
              e flatMap { ee =>
                val (k, v) = c
                v match {
                  case Change.Add(_) if fromTable.get(k).nonEmpty => Left(Problems.UnexpectedEntry(k))
                  case add@Change.Add(_) => add.patch() map (v => Vector(k -> v))
                  case _ if fromTable.get(k).isEmpty => Left(Problems.MissingEntry(k))
                  case remove@Change.Remove(_) => remove.patch(fromTable(k)) map (_ => Vector())
                  case update@Update() => update.patch(fromTable(k)) map (v => Vector(k -> v))
                }
              }
            } map (e => Table(e: _*))
          }
      }

  }

}
