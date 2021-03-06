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
abstract class DataProblem private[data]

/**
 * Concrete data problem implementations.
 */
object DataProblem {

  /** The base class of patching problems. */
  type Patch = patch_data.PatchProblem

  /** The factory for patching problems. */
  val Patch: patch_data.PatchProblem.type = patch_data.PatchProblem

}

