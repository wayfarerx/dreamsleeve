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
package patching

import scala.language.implicitConversions

/**
 * Patching support for the delete factory object.
 */
trait Deletes {

  /**
   * Wraps any deletion with the patching interface.
   *
   * @param delete The deletion to wrap.
   * @return The patching interface.
   */
  @inline
  implicit def deleteToPatch(delete: Difference.Delete): Deletes.Patch =
  new Deletes.Patch(delete)

}

/**
 * Definitions associated with delete patching.
 */
object Deletes {

  /**
   * The patching interface for deletions.
   *
   * @param delete The deletion to provide the patching interface for.
   */
  final class Patch(val delete: Difference.Delete) extends AnyVal {

    /**
     * Patches the difference by verifying the deleted document.
     *
     * @param document The document to verify the deletion of.
     * @param hasher   The hasher to generate hashes with.
     * @return True if the deleted document was verified, false otherwise.
     */
    @inline
    def patch(document: Document)(implicit hasher: Hasher): Boolean =
    document.hash == delete.fromHash

  }

}

