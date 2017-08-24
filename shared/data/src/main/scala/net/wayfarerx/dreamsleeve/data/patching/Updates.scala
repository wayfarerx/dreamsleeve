/*
 * Updates.scala
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

import Problem.Context

/**
 * Patching support for the update factory object.
 */
trait Updates {

  /**
   * Wraps any update with the patching interface.
   *
   * @param update The update to wrap.
   * @return The patching interface.
   */
  @inline
  implicit def updateToPatch(update: Update): Updates.Patch =
  new Updates.Patch(update)

}

/**
 * Definitions associated with update patching.
 */
object Updates {

  /**
   * The patching interface for updates.
   *
   * @param update The update to provide the patching interface for.
   */
  final class Patch(val update: Update) extends AnyVal {

    /**
     * Applies the change to the original fragment, creating the resulting fragment.
     *
     * @param fromFragment The fragment that is being updated.
     * @param hasher       The hasher to generate hashes with.
     * @return The resulting fragment or problems that were encountered updating the fragment.
     */
    def patch(fromFragment: Fragment)(implicit hasher: Hasher): Result[Fragment] =
      patching(fromFragment)(hasher, Context(Vector()))

    /**
     * Applies the change to the original fragment, creating the resulting fragment.
     *
     * @param fromFragment The fragment that is being updated.
     * @param h            The hasher to generate hashes with.
     * @param c            The context of the update application.
     * @return The resulting fragment or problems that were encountered updating the fragment.
     */
    private[patching] def patching(fromFragment: Fragment)(implicit h: Hasher, c: Context): Attempt[Fragment] =
      update match {
        case uc@Update.Copy(_) => uc.patching(fromFragment)
        case ur@Update.Replace(_, _) => ur.patching(fromFragment)
        case um@Update.Modify(_, _) => um.patching(fromFragment)
      }

  }

}
