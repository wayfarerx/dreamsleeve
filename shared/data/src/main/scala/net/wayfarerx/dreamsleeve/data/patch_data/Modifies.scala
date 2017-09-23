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
package patch_data

import cats.Eval

/**
 * Patching support for the modify factory object.
 */
trait Modifies extends PatchFactory[Update.Modify, Fragment, Table] {

  /* Return the modify support object. */
  final override protected def patchSupport: PatchSupport[Update.Modify, Fragment, Table] = Modifies

}

/**
 * Support for modify patching.
 */
object Modifies extends PatchSupport[Update.Modify, Fragment, Table] {

  /* Construct a patch operation for the specified action and data. */
  override def patch(action: Update.Modify, data: Fragment): Eval[PatchResult[Table]] =
    if (action.fromHash == data.hash) {
      data match {
        case v@Value() =>
          Eval.now(Left(PatchProblem.TypeMismatch(v)))
        case fromTable@Table(_) =>
          val expected = action.changes.keySet
          val found = fromTable.keys
          if ((found -- expected).nonEmpty) Eval.now(Left(PatchProblem.MismatchedEntries(found -- expected)))
          else {
            val empty: PatchResult[Vector[(Value, Fragment)]] = Right(Vector())
            (Eval.now(empty) /: action.changes) (patchEntry(fromTable)) map (_ map (r => Table(r: _*)))
          }
      }
    } else Eval.now(Left(PatchProblem.HashMismatch(action.fromHash, data.hash)))

  /**
   * Patches a single entry in a table.
   *
   * @param table    The table to patch.
   * @param previous The outcome of the previously patched entries.
   * @param entry    The entry to patch.
   * @return The previous outcome with the specified entry appended.
   */
  private def patchEntry(table: Table)(previous: Eval[PatchResult[Vector[(Value, Fragment)]]], entry: (Value, Change)) =
    for {
      p <- previous
      (key, change) = entry
      r <- p match {
        case Right(list) => (change match {
          case Change.Add(_) if table.get(key).nonEmpty => Eval.now(Left(PatchProblem.UnexpectedEntry(key)))
          case a@Change.Add(_) => Adds.patch(a, ()) map (_ map (v => Vector(key -> v)))
          case _ if table.get(key).isEmpty => Eval.now(Left(PatchProblem.MissingEntry(key)))
          case r@Change.Remove(_) => Removes.patch(r, table(key)) map (_ map (_ => Vector()))
          case u@Update() => Updates.patch(u, table(key)) map (_ map (v => Vector(key -> v)))
        }) map (_ map (list ++ _))
        case left => Eval.now(left)
      }
    } yield r

}
