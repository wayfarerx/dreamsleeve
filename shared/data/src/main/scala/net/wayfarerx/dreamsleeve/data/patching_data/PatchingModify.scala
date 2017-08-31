/*
 * PatchingModify.scala
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

/**
 * Support for the modification factory object.
 */
trait PatchingModify extends PatchingFactory[Update.Modify, Fragment, Table] {

  /* Return the modify support object. */
  final override protected def patchingSupport: PatchingSupport = PatchingModify

}

/**
 * Definitions associated with modify patching.
 */
object PatchingModify extends PatchingSupport[Update.Modify, Fragment, Table] {

  /* Construct a patcher for the specified action and data. */
  override def apply(action: Update.Modify, data: Fragment): Patching[Table] = for {
    _ <- validateHash(action.fromHash, data)
    table <- data match {
      case v@Value() => report(PatchingProblem.TypeMismatch(v))
      case fromTable@Table(_) => for {
        _ <- validateKeys(action.changes.keySet, fromTable.keys)
        table <- (pure(Vector[(Value, Fragment)]()) /: action.changes) { (r, c) =>
          val (k, v) = c
          for {
            e <- r
            i <- v match {
              case Change.Add(_) if fromTable.get(k).nonEmpty => report(PatchingProblem.UnexpectedEntry(k))
              case a@Change.Add(_) => PatchingAdd(a, ()) map (v => Vector(k -> v))
              case _ if fromTable.get(k).isEmpty => report(PatchingProblem.MissingEntry(k))
              case r@Change.Remove(_) => PatchingRemove(r, fromTable(k)) map (_ => Vector())
              case u@Update() => PatchingUpdate(u, fromTable(k)) map (v => Vector(k -> v))
            }
          } yield e ++ i
        } map (e => Table(e: _*))
      } yield table
    }
  } yield table

}
