/*
 * Deletes.scala
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
 * Patching support for the delete factory object.
 */
trait Deletes extends PatchFactory[Difference.Delete, Document, Unit] {

  /* Return the delete support object. */
  final override protected def patchSupport: PatchSupport[Difference.Delete, Document, Unit] = Deletes

}

/**
 * Support for delete patching.
 */
object Deletes extends PatchSupport[Difference.Delete, Document, Unit] {

  /* Construct a patch operation for the specified action and data. */
  override def patch(action: Difference.Delete, data: Document): PatchOperation[Unit] =
    PatchTask.validateHash(action.fromHash, data)

}

