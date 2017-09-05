/*
 * Data.scala
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

import reflect.ClassTag

import cats._
import free.Free
import Free.liftF
import cats.implicits._

/**
 * Base type for data elements.
 */
abstract class Data extends Hashable {

  /** Alias for the language that describes equality operations. */
  final type EqualsOperation[T] = Data.EqualsOperation[T]

  /** Alias for the tasks that make up equality operations. */
  final protected type EqualsTask[T] = Data.EqualsTask[T]

  /**
   * Creates an operation that can calculate equality against another object.
   *
   * @return An operation that can calculate equality against another object.
   */
  final def equalsOperation(that: Any): EqualsOperation[Boolean] =
    calculateEquals(that)

  /* Use the equality operation for comparisons. */
  final override def equals(that: Any): Boolean =
    equalsOperation(that).foldMap(EqualsTask.interpreter()) getOrElse false

  /* Derive the hash code from a prefix of the data hash. */
  final override def hashCode(): Int = {
    val h = Hash.getInternalRepresentation(hash)
    (h(0) & 0x000000FF) << 24 | (h(1) & 0x000000FF) << 16 | (h(2) & 0x000000FF) << 8 | h(3) & 0x000000FF
  }

  /**
   * Calculate the equality operation for this data element.
   *
   * @param that The instance to test against.
   * @return The equality operation for this data element.
   */
  protected def calculateEquals(that: Any): EqualsOperation[Boolean]

  /**
   * Alias for the equals task factory.
   *
   * @return The alias for the equals task factory
   */
  final protected def EqualsTask: Data.EqualsTask.type = Data.EqualsTask

}

/**
 * Types associated with data elements.
 */
object Data {

  /** The language that describes equality operations. */
  type EqualsOperation[T] = Free[EqualsTask, T]

  /**
   * Base class for equality tasks.
   *
   * @tparam T The type returned by this equality task.
   */
  sealed trait EqualsTask[T] {

    /**
     * Applies this task.
     *
     * @return The result of this equality task.
     */
    def apply(): Option[T]

  }

  /**
   * Declarations associated with equality tasks.
   */
  object EqualsTask {

    /**
     * Creates an equality that returns the supplied instance if it is of the specified type.
     *
     * @param instance The instance to return.
     * @tparam T The type of the instance to return.
     * @return An equality that returns the supplied instance if it is of the specified type.
     */
    def ofType[T: ClassTag](instance: Any): EqualsOperation[T] =
      liftF[Data.EqualsTask, T](new EqualsTask[T] {
        override def apply(): Option[T] = implicitly[ClassTag[T]].unapply(instance)
      })

    /**
     * Creates an equality that returns true if the specified instances are equal and nothing otherwise.
     *
     * @param first  The first object to test.
     * @param second THe second object to test.
     * @tparam T The types of the objects to test.
     * @return An equality that returns true if the specified instances are equal and nothing otherwise.
     */
    def areEqual[T: Eq](first: T, second: T): EqualsOperation[Boolean] =
      liftF[Data.EqualsTask, Boolean](new EqualsTask[Boolean] {
        override def apply(): Option[Boolean] = Some(implicitly[Eq[T]].eqv(first, second)) filter identity
      })

    /**
     * Creates an interpreter for equality tasks.
     *
     * @return An interpreter for equality tasks.
     */
    def interpreter(): EqualsTask ~> Option = new (EqualsTask ~> Option) {
      override def apply[T](op: EqualsTask[T]): Option[T] = op()
    }

  }

}