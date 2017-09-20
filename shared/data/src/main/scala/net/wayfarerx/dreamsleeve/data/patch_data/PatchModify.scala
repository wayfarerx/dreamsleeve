/*
 * PatchModify.scala
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

/**
 * Patching support for the modify factory object.
 */
trait PatchModify extends PatchFactory[Update.Modify, Fragment, Table] {

  /* Return the modify support object. */
  final override protected def patchSupport: PatchSupport[Update.Modify, Fragment, Table] = PatchModify

}

/**
 * Support for modify patching.
 */
object PatchModify extends PatchSupport[Update.Modify, Fragment, Table] {

  /* Construct a patch operation for the specified action and data. */
  override def patch(action: Update.Modify, data: Fragment): PatchOperation[Table] = for {
    _ <- PatchTask.validateHash(action.fromHash, data)
    table <- data match {
      case v@Value() => PatchTask.report(PatchProblem.TypeMismatch(v))
      case fromTable@Table(_) => for {
        _ <- PatchTask.validateKeys(action.changes.keySet, fromTable.keys)
        table <- (PatchTask.pure(Vector[(Value, Fragment)]()) /: action.changes) { (r, c) =>
          val (k, v) = c
          for {
            e <- r
            i <- v match {
              case Change.Add(_) if fromTable.get(k).nonEmpty => PatchTask.report(PatchProblem.UnexpectedEntry(k))
              case a@Change.Add(_) => PatchAdd.patch(a, ()) map (v => Vector(k -> v))
              case _ if fromTable.get(k).isEmpty => PatchTask.report(PatchProblem.MissingEntry(k))
              case r@Change.Remove(_) => PatchRemove.patch(r, fromTable(k)) map (_ => Vector())
              case u@Update() => PatchUpdate.patch(u, fromTable(k)) map (v => Vector(k -> v))
            }
          } yield e ++ i
        } map (e => Table(e: _*))
      } yield table
    }
  } yield table

}
