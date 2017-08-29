/*
 * Removes.scala
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

import language.implicitConversions

/**
 * Support for the removal factory object.
 */
trait Removes {

  /**
   * Wraps any removal with the patching interface.
   *
   * @param remove The removal to wrap.
   * @return The patching interface.
   */
  @inline
  implicit def removeToRemovePatch(remove: Change.Remove): Removes.Patch =
  new Removes.Patch(remove)

}

/**
 * Definitions associated with remove patching.
 */
object Removes {

  /**
   * The patching interface for removals.
   *
   * @param remove The removal to provide the patching interface for.
   */
  final class Patch(val remove: Change.Remove) extends AnyVal {

    /**
     * Applies the change by verifying the removed fragment.
     *
     * @param fromFragment The fragment to verify the removal of.
     * @param hasher       The hasher to generate hashes with.
     * @return Any problem that was encountered removing the fragment, if any.
     */
    def patch(fromFragment: Fragment)(implicit hasher: Hasher): Either[Problems, Unit] =
      if (fromFragment.hash == remove.fromHash) Right(()) else
        Left(Problems.HashMismatch(remove.fromHash, fromFragment.hash))

  }

}
