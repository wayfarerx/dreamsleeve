/*
 * Adds.scala
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
 * Patching support for the add factory object.
 */
trait Adds extends PatchFactory[Change.Add, Unit, Fragment] {

  /* Return the add support object. */
  final override protected def patchSupport: PatchSupport[Change.Add, Unit, Fragment] = Adds

}

/**
 * Support for add patching.
 */
object Adds extends PatchSupport[Change.Add, Unit, Fragment] {

  /* Construct a patch operation for the specified action and data. */
  override def patch(action: Change.Add, data: Unit): Eval[PatchResult[Fragment]] =
    Eval.now(Right(action.toFragment))

}
