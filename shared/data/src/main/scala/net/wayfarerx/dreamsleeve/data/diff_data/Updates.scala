/*
 * Updates.scala
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
package diff_data

/**
 * Diffing support for the update factory object.
 */
trait Updates extends DiffFactory[Fragment, Update] {

  /* Return the update support object. */
  final override protected def diffSupport: DiffSupport[Fragment, Update] = Updates

}

/**
 * Support for update patching.
 */
object Updates extends DiffSupport[Fragment, Update] {

  /* Construct a differ for the specified original and resulting data. */
  override def diff(fromData: Fragment, toData: Fragment): DiffOperation[Update] = (fromData, toData) match {
    case (f, t) if f == t => DiffTask.createCopy[Update](f)
    case ((Value(), _) | (_, Value())) => DiffTask.createReplace[Update](fromData, toData)
    case (fromTable@Table(_), toTable@Table(_)) => for {
      changes <- (DiffTask.pure(Vector[(Value, Change)]()) /: (fromTable.keys ++ toTable.keys).toVector) { (r, k) =>
        for {
          e <- r
          c <- (fromTable get k, toTable get k) match {
            case (Some(fv), None) => DiffTask.createRemove[Change](fv)
            case (None, Some(tv)) => DiffTask.createAdd[Change](tv)
            case _ => diff(fromTable(k), toTable(k))
          }
        } yield e :+ (k, c)
      }
      result <- DiffTask.createModify[Update](fromTable, changes)
    } yield result
  }

}
