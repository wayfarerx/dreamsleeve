/*
 * DataProblem.scala
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

/**
 * Base class for all data problems.
 */
trait DataProblem

/**
 * Concrete data problem implementations.
 */
object DataProblem {

  /** The base class of patching problems. */
  type Patching = patching_data.PatchingProblem

  /** The factory for patching problems. */
  val Patching: patching_data.PatchingProblem.type = patching_data.PatchingProblem

  /** The base class of textual problems. */
  type Binary = binary_data.Problems

  /** The factory for textual problems. */
  val Binary: binary_data.Problems.type = binary_data.Problems

  /** The base class of textual problems. */
  type Textual = textual_data.TextualProblem

  /** The factory for textual problems. */
  val Textual: textual_data.TextualProblem.type = textual_data.TextualProblem

}
