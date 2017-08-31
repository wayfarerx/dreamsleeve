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
package object patching_data {

  /** The type of operations that apply patches to data. */
  type Patching[R] = Free[PatchingOperation, R]

  /** The type of result returned from patching operations. */
  type PatchingResult[R] = Either[PatchingProblem, R]

  /**
   * Base class for factory mix ins that support patching operations.
   *
   * @tparam A The type of the supported action.
   * @tparam D The type of the data input.
   * @tparam R The result type of the patching operation.
   */
  trait PatchingFactory[A, D, R] {

    /** The type of support object used by this factory. */
    final protected type PatchingSupport = patching_data.PatchingSupport[A, D, R]

    /**
     * Wraps an action with extensions that support patching operations.
     *
     * @param action The action item to extend.
     * @return The specified action item wrapped with extensions that support patching operations.
     */
    final implicit def patchingActionToPatchingExtensions(action: A): PatchingExtensions[D, R] =
      new PatchingExtensions[D, R](patchingSupport(action, _))

    /**
     * Returns the support interface for this factory.
     *
     * @return The support interface for this factory.
     */
    protected def patchingSupport: PatchingSupport

  }

  /**
   * Support interface for patching operations.
   *
   * @tparam A The type of the supported action.
   * @tparam D The type of the data input.
   * @tparam R The result type of the patching operation.
   */
  trait PatchingSupport[A, D, R] {

    /**
     * Creates a patcher for the specified action and data.
     *
     * @param action The action item to create a patcher for.
     * @param data   The data to be patched.
     * @return A patcher for the specified action and data.
     */
    def apply(action: A, data: D): Patching[R]

    /**
     * Creates a patcher that returns the specified result.
     *
     * @param result The result to return.
     * @tparam T The type of the expected result.
     * @return A patcher that returns the specified result.
     */
    final protected def pure[T](result: T): Patching[T] =
      liftF[PatchingOperation, T](_ => Right(result))

    /**
     * Creates a patcher that fails with the specified problem.
     *
     * @param problem The problem to fail with.
     * @tparam T The type of the expected result.
     * @return A patcher that fails with the specified problem.
     */
    final protected def report[T](problem: PatchingProblem): Patching[T] =
      liftF[PatchingOperation, T](_ => Left(problem))

    /**
     * Creates a patcher that validates that two hashes are the same.
     *
     * @param expected The hash that is expected.
     * @param found    The hashable that was found.
     * @return A patcher that validates that two hashes are the same.
     */
    final protected def validateHash(expected: Hash, found: Hashable): Patching[Unit] =
      liftF[PatchingOperation, Unit] { hasher =>
        val foundHash = found.hash(hasher)
        if (expected == foundHash) Right(())
        else Left(PatchingProblem.HashMismatch(expected, foundHash))
      }

    /**
     * Creates a patcher that validates that two collections of keys are the same.
     *
     * @param expected The collections of keys that is expected.
     * @param found    The collections of keys that was found.
     * @return A patcher that validates that two collections of keys are the same.
     */
    final protected def validateKeys(expected: SortedSet[Value], found: SortedSet[Value]): Patching[Unit] =
      liftF[PatchingOperation, Unit] { _ =>
        if ((found -- expected).nonEmpty) Left(PatchingProblem.MismatchedEntries(found -- expected))
        else Right(())
      }

  }

  /**
   * Extensions that add support for patching operations.
   *
   * @param dataToPatching The function that returns a patcher for the supplied data.
   */
  final class PatchingExtensions[D, R] private[patching_data](val dataToPatching: D => Patching[R]) extends AnyVal {

    /**
     * Patches the supplied data with the underlying action.
     *
     * @param data   The data item to patch.
     * @param hasher The hasher to use during the patching operation.
     * @return The result of patching the supplied data with the underlying action.
     */
    def patch(data: D)(implicit hasher: Hasher): PatchingResult[R] =
      dataToPatching(data).foldMap(PatchingOperation(hasher))

  }

  /**
   * Base class for operation that patch data.
   *
   * @tparam R The type returned by this operation.
   */
  trait PatchingOperation[R] {

    /**
     * Applies this operation using the specified hasher.
     *
     * @param hasher The hasher to use while patching.
     * @return The result of this patching operation.
     */
    def apply(hasher: Hasher): PatchingResult[R]

  }

  /**
   * Declarations associated with patching operations.
   */
  object PatchingOperation {

    /**
     * Creates an interpreter for patching operations.
     *
     * @param hasher The hasher to use while patching.
     * @return A new interpreter for patching operations.
     */
    def apply(hasher: Hasher): PatchingOperation ~> PatchingResult = new (PatchingOperation ~> PatchingResult) {
      override def apply[R](op: PatchingOperation[R]): PatchingResult[R] = op(hasher)
    }

  }

}
