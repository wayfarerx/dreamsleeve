/*
 * DiffingRevise.scala
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
package diffing_data

/**
 * Diffing support for the revise factory object.
 */
trait DiffingRevise extends DiffingFactory[Document, Difference.Revise] {

  /* Return the revise support object. */
  final override protected def diffingSupport: DiffingSupport = DiffingRevise

}

/**
 * Definitions associated with revise diffing.
 */
object DiffingRevise extends DiffingSupport[Document, Difference.Revise] {

  /* Construct a differ for the specified original and resulting data. */
  override def apply(fromData: Document, toData: Document): Diffing[Difference.Revise] = for {
    update <- DiffingUpdate(fromData.content, toData.content)
    result <- createRevise(fromData, toData.title, update)
  } yield result

}
