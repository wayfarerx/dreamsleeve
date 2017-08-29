/*
 * Copies.scala
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
 * Support for the copies factory object.
 */
trait Copies {

  /**
   * Wraps any copy with the patching interface.
   *
   * @param copy The copy to wrap.
   * @return The patching interface.
   */
  @inline
  implicit def copyToCopiesPatch(copy: Update.Copy): Copies.Patch =
  new Copies.Patch(copy)

}

/**
 * Definitions associated with copy patching.
 */
object Copies {

  /**
   * The patching interface for copies.
   *
   * @param copy The copy to provide the patching interface for.
   */
  final class Patch(val copy: Update.Copy) extends AnyVal {

    /**
     * Applies the update by verifying and returning the fragment.
     *
     * @param fromFragment The fragment that is being copied.
     * @param hasher       The hasher to generate hashes with.
     * @return The original fragment or any problem that was encountered copying the fragment.
     */
    def patch(fromFragment: Fragment)(implicit hasher: Hasher): Either[Problems, Fragment] =
      if (copy.theHash == fromFragment.hash) Right(fromFragment) else
        Left(Problems.HashMismatch(copy.theHash, fromFragment.hash))

  }

}
