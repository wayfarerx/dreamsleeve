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

import cats.~>
import cats.Id
import cats.free.Free
import Free.liftF

/**
 * Global definitions for the diffing package.
 */
package object diffing_data {

  /** The type of operations that diff data. */
  type Diffing[R] = Free[DiffingOperation, R]

  /**
   * Base class for factory mix ins that support diffing operations.
   *
   * @tparam D The type of the data inputs.
   * @tparam R The result type of the diffing operation.
   */
  trait DiffingFactory[D, R] {

    /** The type of support object used by this factory. */
    final protected type DiffingSupport = diffing_data.DiffingSupport[D, R]

    /**
     * Creates an update by collecting the differences between two fragments.
     *
     * @param from   The fragment that is being updated.
     * @param to     The fragment that is a result of the update.
     * @return An update collecting the differences between two fragments.
     */
    final def apply(from: D, to: D): R =
      diffingSupport(from, to).foldMap(DiffingOperation.interpreter)

    /**
     * Returns the support interface for this factory.
     *
     * @return The support interface for this factory.
     */
    protected def diffingSupport: DiffingSupport

  }

  /**
   * Support interface for diffing operations.
   *
   * @tparam D The type of the data inputs.
   * @tparam R The result type of the diffing operation.
   */
  trait DiffingSupport[D, R] {

    /**
     * Creates a differ for the specified original and resulting data.
     *
     * @param fromData The original data.
     * @param toData   The resulting data.
     * @return A differ for the specified original and resulting data.
     */
    def apply(fromData: D, toData: D): Diffing[R]

    /**
     * Creates a differ that returns the specified result.
     *
     * @param result The result to return.
     * @tparam T The type of the expected result.
     * @return A differ that returns the specified result.
     */
    final protected def pure[T](result: T): Diffing[T] =
      liftF[DiffingOperation, T](() => result)

    /**
     * Creates a differ that returns an add operation.
     *
     * @param to The fragment being added.
     * @tparam T The desired return type.
     * @return A differ that returns an add operation.
     */
    final protected def createAdd[T >: Change.Add](to: Fragment): Diffing[T] =
      liftF[DiffingOperation, T](() => Change.Add(to))

    /**
     * Creates a differ that returns a remove operation.
     *
     * @param from The fragment being removed.
     * @tparam T The desired return type.
     * @return A differ that returns a remove operation.
     */
    final protected def createRemove[T >: Change.Remove](from: Fragment): Diffing[T] =
      liftF[DiffingOperation, T](() => Change.Remove(from))

    /**
     * Creates a differ that returns a copy operation.
     *
     * @param from The fragment being copied.
     * @tparam T The desired return type.
     * @return A differ that returns a copy operation.
     */
    final protected def createCopy[T >: Update.Copy](from: Fragment): Diffing[T] =
      liftF[DiffingOperation, T](() => Update.Copy(from))

    /**
     * Creates a differ that returns a replace operation.
     *
     * @param from The fragment being replaced.
     * @param to   The fragment doing the replacing replaced.
     * @tparam T The desired return type.
     * @return A differ that returns a replace operation.
     */
    final protected def createReplace[T >: Update.Replace](from: Fragment, to: Fragment): Diffing[T] =
      liftF[DiffingOperation, T](() => Update.Replace(from, to))

    /**
     * Creates a differ that returns a modify operation.
     *
     * @param from      The table being modified.
     * @param toChanges The changes to apply to the table.
     * @tparam T The desired return type.
     * @return A differ that returns a modify operation.
     */
    final protected def createModify[T >: Update.Modify](from: Table, toChanges: Seq[(Value, Change)]): Diffing[T] =
      liftF[DiffingOperation, T](() => Update.Modify(from, toChanges: _*))

    /**
     * Creates a differ that returns a revise operation.
     *
     * @param from     The document being revised.
     * @param toTitle  The title of the resulting document.
     * @param toUpdate The update to apply to the original content.
     * @return A differ that returns a revise operation.
     */
    final protected def createRevise(from: Document, toTitle: String, toUpdate: Update): Diffing[Difference.Revise] =
      liftF[DiffingOperation, Difference.Revise](() => Difference.Revise(from, toTitle, toUpdate))

  }

  /**
   * Base class for operation that diff data.
   *
   * @tparam R The type returned by this operation.
   */
  trait DiffingOperation[R] {

    /**
     * Applies this operation.
     *
     * @return The result of this diffing operation.
     */
    def apply(): R

  }

  /**
   * Declarations associated with diffing operations.
   */
  object DiffingOperation {

    /** The interpreter for diffing operations. */
    val interpreter: DiffingOperation ~> Id = new (DiffingOperation ~> Id) {
      override def apply[R](op: DiffingOperation[R]): Id[R] = op()
    }

  }

}
