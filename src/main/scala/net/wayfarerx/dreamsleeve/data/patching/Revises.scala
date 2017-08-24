/*
 * Revises.scala
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
package patching

import scala.language.implicitConversions

import cats.data.Validated.{invalid, valid}

/**
 * Patching support for the revise factory object.
 */
trait Revises {

  /**
   * Wraps any revision with the patching interface.
   *
   * @param revise The revision to wrap.
   * @return The patching interface.
   */
  @inline
  implicit def reviseToPatch(revise: Difference.Revise): Revises.Patch =
  new Revises.Patch(revise)

}

/**
 * Definitions associated with revise patching.
 */
object Revises {

  /**
   * The patching interface for revisions.
   *
   * @param revise The revision to provide the patching interface for.
   */
  final class Patch(val revise: Difference.Revise) extends AnyVal {

    /**
     * Applies this difference by applying all changes to the specified document.
     *
     * @param fromDocument The document to apply changes to.
     * @param hasher       The hasher to generate hashes with.
     * @return Either the resulting document after applying changes to the original document or any problems that were
     *         encountered.
     */
    def patch(fromDocument: Document)(implicit hasher: Hasher): Result[Document] = {
      val hashCheck = Some(revise.fromHash) filter (_ != fromDocument.hash) map
        (Problems.HashMismatch(_, fromDocument.hash)(Problem.Context(Vector())))
      revise.update.patching(fromDocument.content)(hasher, Problem.Context(Vector(Value.String(revise.title))))
        .leftMap(e => hashCheck map (_ +: e.toList.toVector) getOrElse e.toList.toVector)
        .andThen(c => hashCheck map (p => invalid(Vector(p))) getOrElse valid(Document(revise.title, c)))
        .toEither
    }

  }

}

