/*
 * Replaces.scala
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
 * Patching support for the replace factory object.
 */
trait Replaces extends PatchFactory[Update.Replace, Fragment, Fragment] {

  /* Return the replace support object. */
  final override protected def patchSupport: PatchSupport[Update.Replace, Fragment, Fragment] = Replaces

}

/**
 * Support for replace patching.
 */
object Replaces extends PatchSupport[Update.Replace, Fragment, Fragment] {

  /* Construct a patch operation for the specified action and data. */
  override def patch(action: Update.Replace, data: Fragment): Eval[PatchResult[Fragment]] =
    if (action.fromHash == data.hash) Eval.now(Right(action.toFragment))
    else Eval.now(Left(PatchProblem.HashMismatch(action.fromHash, data.hash)))

}
