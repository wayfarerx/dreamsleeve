/*
 * PatchingRemove.scala
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
 * Support for the removal factory object.
 */
trait PatchingRemove extends PatchingFactory[Change.Remove, Fragment, Unit] {

  /* Return the add support object. */
  override protected def patchingSupport: PatchingSupport = PatchingRemove

}

/**
 * Definitions associated with remove patching.
 */
object PatchingRemove extends PatchingSupport[Change.Remove, Fragment, Unit] {

  /* Construct a patcher for the specified action and data. */
  override def apply(action: Change.Remove, data: Fragment): Patching[Unit] =
    validateHash(action.fromHash, data)

}
