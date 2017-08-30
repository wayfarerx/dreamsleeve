/*
 * PatchingCreate.scala
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
 * Patching support for the create factory object.
 */
trait PatchingCreate extends PatchingFactory[Difference.Create, Unit, Document] {

  /* Return the add support object. */
  override protected def patchingSupport: PatchingSupport = PatchingCreate

}

/**
 * Definitions associated with create patching.
 */
object PatchingCreate extends PatchingSupport[Difference.Create, Unit, Document] {

  /* Construct a patcher for the specified action and data. */
  override def apply(action: Difference.Create, data: Unit): Patching[Document] =
    pure(action.document)

}
