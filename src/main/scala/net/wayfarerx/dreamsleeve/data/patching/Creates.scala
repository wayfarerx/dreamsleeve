/*
 * Creates.scala
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

/**
 * Patching support for the create factory object.
 */
trait Creates {

  /**
   * Wraps any creation with the patching interface.
   *
   * @param create The creation to wrap.
   * @return The patching interface.
   */
  @inline
  implicit def createToPatch(create: Difference.Create): Creates.Patch =
  new Creates.Patch(create)

}

/**
 * Definitions associated with create patching.
 */
object Creates {

  /**
   * The patching interface for creations.
   *
   * @param create The creation to provide the patching interface for.
   */
  final class Patch(val create: Difference.Create) extends AnyVal {

    /**
     * Patches the difference by returning the created document.
     *
     * @return The created document.
     */
    @inline
    def patch(): Document =
    create.document

  }

}
