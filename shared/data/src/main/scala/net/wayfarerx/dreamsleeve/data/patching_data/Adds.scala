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
package patching_data

import language.implicitConversions

/**
 * Support for the addition factory object.
 */
trait Adds {

  /**
   * Wraps any addition with the patching interface.
   *
   * @param add The addition to wrap.
   * @return The patching interface.
   */
  @inline
  implicit def addToAddPatch(add: Change.Add): Adds.Patch =
  new Adds.Patch(add)

}

/**
 * Definitions associated with add patching.
 */
object Adds {

  /**
   * The patching interface for additions.
   *
   * @param add The addition to provide the patching interface for.
   */
  final class Patch(val add: Change.Add) extends AnyVal {

    /**
     * Applies the change by returning the fragment to add.
     *
     * @return The fragment to add or problems that were encountered adding the fragment.
     */
    def patch(): Either[Problems, Fragment] =
      Right(add.toFragment)

  }

}
