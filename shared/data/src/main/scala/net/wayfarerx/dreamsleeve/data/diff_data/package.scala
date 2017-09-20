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
package object diff_data {

  /** The type of operations that diff data. */
  type DiffOperation[T] = Free[DiffTask, T]

  /**
   * Base class for factory mix ins that support diffing operations.
   *
   * @tparam I The type of the data inputs.
   * @tparam O The result type of the diffing operation.
   */
  trait DiffFactory[I, O] {

    /**
     * Creates an update by collecting the differences between two fragments.
     *
     * @param from The fragment that is being updated.
     * @param to   The fragment that is a result of the update.
     * @return An update collecting the differences between two fragments.
     */
    final def apply(from: I, to: I): O =
      diffSupport.diff(from, to).foldMap(DiffTask.Interpreter)

    /**
     * Returns the support interface for this factory.
     *
     * @return The support interface for this factory.
     */
    protected def diffSupport: DiffSupport[I, O]

  }

  /**
   * Support interface for diffing operations.
   *
   * @tparam I The type of the data inputs.
   * @tparam O The result type of the diffing operation.
   */
  trait DiffSupport[I, O] {

    /**
     * Creates a differ for the specified original and resulting data.
     *
     * @param fromData The original data.
     * @param toData   The resulting data.
     * @return A differ for the specified original and resulting data.
     */
    def diff(fromData: I, toData: I): DiffOperation[O]

  }

  /**
   * Base class for tasks that diff data.
   *
   * @tparam T The type returned by this task.
   */
  sealed trait DiffTask[T] {

    /**
     * Applies this task.
     *
     * @return The result of this diffing task.
     */
    def apply(): T

  }

  /**
   * Declarations associated with diffing tasks.
   */
  object DiffTask {

    /** The interpreter for diffing tasks. */
    val Interpreter: DiffTask ~> Id = new (DiffTask ~> Id) {
      override def apply[R](op: DiffTask[R]): Id[R] = op()
    }

    /**
     * Creates a differ that returns the specified result.
     *
     * @param result The result to return.
     * @tparam T The type of the expected result.
     * @return A differ that returns the specified result.
     */
    def pure[T](result: T): DiffOperation[T] =
      liftF[DiffTask, T](new DiffTask[T] {
        override def apply(): T = result
      })

    /**
     * Creates a differ that returns an add operation.
     *
     * @param to The fragment being added.
     * @tparam T The desired return type.
     * @return A differ that returns an add operation.
     */
    def createAdd[T >: Change.Add](to: Fragment): DiffOperation[T] =
      liftF[DiffTask, T](new DiffTask[T] {
        override def apply(): T = Change.Add(to)
      })

    /**
     * Creates a differ that returns a remove operation.
     *
     * @param from The fragment being removed.
     * @tparam T The desired return type.
     * @return A differ that returns a remove operation.
     */
    def createRemove[T >: Change.Remove](from: Fragment): DiffOperation[T] =
      liftF[DiffTask, T](new DiffTask[T] {
        override def apply(): T = Change.Remove(from)
      })

    /**
     * Creates a differ that returns a copy operation.
     *
     * @param from The fragment being copied.
     * @tparam T The desired return type.
     * @return A differ that returns a copy operation.
     */
    def createCopy[T >: Update.Copy](from: Fragment): DiffOperation[T] =
      liftF[DiffTask, T](new DiffTask[T] {
        override def apply(): T = Update.Copy(from)
      })

    /**
     * Creates a differ that returns a replace operation.
     *
     * @param from The fragment being replaced.
     * @param to   The fragment doing the replacing replaced.
     * @tparam T The desired return type.
     * @return A differ that returns a replace operation.
     */
    def createReplace[T >: Update.Replace](from: Fragment, to: Fragment): DiffOperation[T] =
      liftF[DiffTask, T](new DiffTask[T] {
        override def apply(): T = Update.Replace(from, to)
      })

    /**
     * Creates a differ that returns a modify operation.
     *
     * @param from      The table being modified.
     * @param toChanges The changes to apply to the table.
     * @tparam T The desired return type.
     * @return A differ that returns a modify operation.
     */
    def createModify[T >: Update.Modify](from: Table, toChanges: Seq[(Value, Change)]): DiffOperation[T] =
      liftF[DiffTask, T](new DiffTask[T] {
        override def apply(): T = Update.Modify(from, toChanges: _*)
      })

    /**
     * Creates a differ that returns a revise operation.
     *
     * @param from     The document being revised.
     * @param toTitle  The title of the resulting document.
     * @param toUpdate The update to apply to the original content.
     * @return A differ that returns a revise operation.
     */
    def createRevise(from: Document, toTitle: String, toUpdate: Update): DiffOperation[Difference.Revise] =
      liftF[DiffTask, Difference.Revise](new DiffTask[Difference.Revise] {
        override def apply(): Difference.Revise = Difference.Revise(from, toTitle, toUpdate)
      })

  }

}
