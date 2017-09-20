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

import collection.immutable.SortedSet
import language.implicitConversions

import cats.~>
import cats.free.Free
import Free.liftF
import cats.implicits._

/**
 * Global definitions for the patching package.
 */
package object patch_data {

  /** The type of operations that apply patches to data. */
  type PatchOperation[T] = Free[PatchTask, T]

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
    def patch(action: T, data: I): PatchOperation[O]

  }

  /**
   * Extensions that add support for patching operations.
   *
   * @param dataToPatch The function that returns a patch operation for the supplied data.
   */
  final class PatchExtensions[D, R] private[patch_data](val dataToPatch: D => PatchOperation[R]) extends AnyVal {

    /**
     * Patches the supplied data with the underlying action.
     *
     * @param data The data item to patch.
     * @return The result of patching the supplied data with the underlying action.
     */
    def patch(data: D)(): PatchResult[R] =
      dataToPatch(data).foldMap(PatchTask.Interpreter)

  }

  /**
   * Base class for tasks that patch data.
   *
   * @tparam R The type returned by this task.
   */
  sealed trait PatchTask[R] {

    /**
     * Applies this task.
     *
     * @return The result of this patch task.
     */
    def apply(): PatchResult[R]

  }

  /**
   * Declarations associated with patch tasks.
   */
  object PatchTask {

    /** The interpreter for patch operations. */
    val Interpreter: PatchTask ~> PatchResult = new (PatchTask ~> PatchResult) {
      override def apply[R](op: PatchTask[R]): PatchResult[R] = op()
    }

    /**
     * Creates a patch operation that returns the specified result.
     *
     * @param result The result to return.
     * @tparam T The type of the expected result.
     * @return A patch operation that returns the specified result.
     */
    def pure[T](result: T): PatchOperation[T] =
      liftF[PatchTask, T](new PatchTask[T] {
        override def apply(): PatchResult[T] = Right(result)
      })

    /**
     * Creates a patch operation that fails with the specified problem.
     *
     * @param problem The problem to fail with.
     * @tparam T The type of the expected result.
     * @return A patch operation that fails with the specified problem.
     */
    def report[T](problem: PatchProblem): PatchOperation[T] =
      liftF[PatchTask, T](new PatchTask[T] {
        override def apply(): PatchResult[T] = Left(problem)
      })

    /**
     * Creates a patch operation that validates that two hashes are the same.
     *
     * @param expected The hash that is expected.
     * @param found    The hashable that was found.
     * @return A patch operation that validates that two hashes are the same.
     */
    def validateHash(expected: Hash, found: Data): PatchOperation[Unit] =
      liftF[PatchTask, Unit](new PatchTask[Unit] {
        override def apply(): PatchResult[Unit] = {
          val foundHash = found.hash
          if (expected == foundHash) Right(())
          else Left(PatchProblem.HashMismatch(expected, foundHash))
        }
      })

    /**
     * Creates a patch operation that validates that two collections of keys are the same.
     *
     * @param expected The collections of keys that is expected.
     * @param found    The collections of keys that was found.
     * @return A patch operation that validates that two collections of keys are the same.
     */
    def validateKeys(expected: SortedSet[Value], found: SortedSet[Value]): PatchOperation[Unit] =
      liftF[PatchTask, Unit](new PatchTask[Unit] {
        override def apply(): PatchResult[Unit] = {
          if ((found -- expected).nonEmpty) Left(PatchProblem.MismatchedEntries(found -- expected))
          else Right(())
        }
      })

  }

}
