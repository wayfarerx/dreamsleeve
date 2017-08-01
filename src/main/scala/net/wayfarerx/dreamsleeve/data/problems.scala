/*
 * problems.scala
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

import cats.data._

/**
 * Base class for all problems.
 */
trait Problem {

  /** The context where this problem occurred. */
  def context: Problem.Context

}

/**
 * Concrete problem implementations.
 */
object Problem {

  /**
   * Base class for problem factory objects.
   *
   * @tparam ProblemType The type of problem managed by this factory.
   */
  trait Factory[ProblemType] {

    /** The context that all problems originate from. */
    final type Context = Problem.Context

    /** The type of a list of problems. */
    final type List = NonEmptyList[ProblemType]

    /** The factory for lists of problems. */
    final val List = NonEmptyList

  }

  /**
   * The context passed among operations that might encounter problems.
   *
   * @param location the current location in the document.
   */
  case class Context private(location: Vector[Value]) {

    /**
     * Returns a context with the specified element at the end of the location.
     *
     * @param element The element to append to the location.
     * @return A context with the specified element at the end of the location.
     */
    private[data] def push(element: Value): Context =
      copy(location = location :+ element)

  }

}

/**
 * Problem implementations and factories.
 */
object Problems {

  /** The base class of patching problems. */
  type Patching = PatchingProblem

  /** The factory for patching problems. */
  val Patching = PatchingProblem

}

