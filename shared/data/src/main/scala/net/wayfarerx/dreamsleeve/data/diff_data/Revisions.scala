/*
 * Revisions.scala
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
package diff_data

/**
 * Diffing support for the revise factory object.
 */
trait Revisions extends DiffFactory[Document, Difference.Revise] {

  /* Return the revise support object. */
  final override protected def diffSupport: DiffSupport[Document, Difference.Revise] = Revisions

}

/**
 * Support for revise patching.
 */
object Revisions extends DiffSupport[Document, Difference.Revise] {

  /* Construct a differ for the specified original and resulting data. */
  override def diff(fromData: Document, toData: Document): DiffOperation[Difference.Revise] = for {
    update <- Updates.diff(fromData.content, toData.content)
    result <- DiffTask.createRevise(fromData, toData.title, update)
  } yield result

}
