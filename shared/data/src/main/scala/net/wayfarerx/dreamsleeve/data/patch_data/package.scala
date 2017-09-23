/*
 * package.scala
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

import language.implicitConversions

import cats.Eval

/**
 * Global definitions for the patching package.
 */
package object patch_data {

  /** The type of result returned from patching operations. */
  type PatchResult[T] = Either[PatchProblem, T]

  /**
   * Base class for factory mix ins that support patching operations.
   *
   * @tparam T The type of the supported action.
   * @tparam I The type of the data input.
   * @tparam O The result type of the patching operation.
   */
  trait PatchFactory[T, I, O] {

    /**
     * Wraps an action with extensions that support patching operations.
     *
     * @param action The action item to extend.
     * @return The specified action item wrapped with extensions that support patching operations.
     */
    final implicit def patchActionToPatchExtensions(action: T): PatchExtensions[I, O] =
      new PatchExtensions[I, O](patchSupport.patch(action, _))

    /**
     * Returns the support interface for this factory.
     *
     * @return The support interface for this factory.
     */
    protected def patchSupport: PatchSupport[T, I, O]

  }

  /**
   * Support interface for patching operations.
   *
   * @tparam T The type of the supported action.
   * @tparam I The type of the data input.
   * @tparam O The result type of the patching operation.
   */
  trait PatchSupport[T, I, O] {

    /**
     * Creates a patch operation for the specified action and data.
     *
     * @param action The action item to create a patch operation for.
     * @param data   The data to be patched.
     * @return A patch operation for the specified action and data.
     */
    def patch(action: T, data: I): Eval[PatchResult[O]]

  }

  /**
   * Extensions that add support for patching operations.
   *
   * @tparam I The type of the data input.
   * @tparam O The result type of the patching operation.
   * @param dataToPatch The function that returns a patch operation for the supplied data.
   */
  final class PatchExtensions[I, O] private[patch_data](val dataToPatch: I => Eval[PatchResult[O]]) extends AnyVal {

    /**
     * Patches the supplied data with the underlying action.
     *
     * @param data The data item to patch.
     * @return The result of patching the supplied data with the underlying action.
     */
    def patch(data: I)(): PatchResult[O] =
      dataToPatch(data).value

  }

}
