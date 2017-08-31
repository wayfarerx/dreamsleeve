/*
 * DiffingUpdate.scala
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
package diffing_data

/**
 * Diffing support for the update factory object.
 */
trait DiffingUpdate extends DiffingFactory[Fragment, Update] {

  /* Return the update support object. */
  final override protected def diffingSupport: DiffingSupport = DiffingUpdate

}

/**
 * Definitions associated with update diffing.
 */
object DiffingUpdate extends DiffingSupport[Fragment, Update] {

  /* Construct a differ for the specified original and resulting data. */
  override def apply(fromData: Fragment, toData: Fragment): Diffing[Update] = (fromData, toData) match {
    case (f, t) if f == t => createCopy[Update](f)
    case ((Value(), _) | (_, Value())) => createReplace[Update](fromData, toData)
    case (fromTable@Table(_), toTable@Table(_)) => for {
      changes <- (pure(Vector[(Value, Change)]()) /: (fromTable.keys ++ toTable.keys).toVector) { (r, k) =>
        for {
          e <- r
          c <- (fromTable get k, toTable get k) match {
            case (Some(fv), None) => createRemove[Change](fv)
            case (None, Some(tv)) => createAdd[Change](tv)
            case _ => apply(fromTable(k), toTable(k))
          }
        } yield e :+ (k, c)
      }
      result <- createModify[Update](fromTable, changes)
    } yield result
  }

}
