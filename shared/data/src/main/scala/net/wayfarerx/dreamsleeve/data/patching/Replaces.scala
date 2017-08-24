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
package patching

import language.implicitConversions

import cats.data._
import Validated.{invalid, valid}

import Problem.Context
import Problems._

/**
 * Support for the replacement factory object.
 */
trait Replaces {

  /**
   * Wraps any replacement with the patching interface.
   *
   * @param replace The replacement to wrap.
   * @return The patching interface.
   */
  @inline
  implicit def replaceToPatch(replace: Update.Replace): Replaces.Patch =
  new Replaces.Patch(replace)

}

/**
 * Definitions associated with replace patching.
 */
object Replaces {

  /**
   * The patching interface for replacements.
   *
   * @param replace The replacement to provide the patching interface for.
   */
  final class Patch(val replace: Update.Replace) extends AnyVal {

    /**
     * Applies the update by verifying the original fragment and returning the resulting fragment.
     *
     * @param fromFragment The fragment that is being replaced.
     * @param hasher       The hasher to generate hashes with.
     * @return The resulting fragment or problems that were encountered replacing the fragment.
     */
    def patch(fromFragment: Fragment)(implicit hasher: Hasher): Result[Fragment] =
      patching(fromFragment)(hasher, Context(Vector()))

    /**
     * Applies the update by verifying the original fragment and returning the resulting fragment.
     *
     * @param fromFragment The fragment that is being replaced.
     * @param h            The hasher to generate hashes with.
     * @param c            The context of the replaced application.
     * @return The resulting fragment or problems that were encountered replacing the fragment.
     */
    private[patching] def patching(fromFragment: Fragment)(implicit h: Hasher, c: Context): Attempt[Fragment] =
      if (replace.fromHash == fromFragment.hash) valid(replace.toFragment) else
        invalid(Problems.List.of(HashMismatch(replace.fromHash, fromFragment.hash)))

  }

}
