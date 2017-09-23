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
package patch_data

import cats.Eval

/**
 * Patching support for the update factory object.
 */
trait Updates extends PatchFactory[Update, Fragment, Fragment] {

  /* Return the update support object. */
  final override protected def patchSupport: PatchSupport[Update, Fragment, Fragment] = Updates

}

/**
 * Support for update patching.
 */
object Updates extends PatchSupport[Update, Fragment, Fragment] {

  /* Construct a patch operation for the specified action and data. */
  override def patch(action: Update, data: Fragment): Eval[PatchResult[Fragment]] = action match {
    case c@Update.Copy(_) => Copies.patch(c, data)
    case r@Update.Replace(_, _) => Replaces.patch(r, data)
    case m@Update.Modify(_, _) => Modifies.patch(m, data) // map (_ map (t => t: Fragment))
  }

}
