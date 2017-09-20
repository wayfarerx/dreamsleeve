/*
 * PatchCopy.scala
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
 * Patching support for the copy factory object.
 */
trait PatchCopy extends PatchFactory[Update.Copy, Fragment, Fragment] {

  /* Return the copy support object. */
  final override protected def patchSupport: PatchSupport[Update.Copy, Fragment, Fragment] = PatchCopy

}

/**
 * Support for copy patching.
 */
object PatchCopy extends PatchSupport[Update.Copy, Fragment, Fragment] {

  /* Construct a patch operation for the specified action and data. */
  override def patch(action: Update.Copy, data: Fragment): PatchOperation[Fragment] = for {
    _ <- PatchTask.validateHash(action.theHash, data)
  } yield data

}
