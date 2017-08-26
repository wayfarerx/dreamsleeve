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
trait Problem

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

    /** The type of a list of problems. */
    final type List = NonEmptyList[ProblemType]

    /** The factory for lists of problems. */
    final val List = NonEmptyList

  }

}

/**
 * Problem implementations and factories.
 */
object Problems {

  /** The base class of patching problems. */
  type Patching = patching.Problems

  /** The factory for patching problems. */
  val Patching = patching.Problems

  /** The base class of textual problems. */
  type Textual = TextualProblem

  /** The factory for textual problems. */
  val Textual = TextualProblem

}

