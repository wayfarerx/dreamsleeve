/*
 * patching.scala
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

import language.implicitConversions
import cats.data._
import cats.implicits._
import Validated.{invalid, valid}

import scala.collection.immutable.SortedSet

/**
 * Provides support for patching documents with differences.
 */
object PatchingDifferences {

  /**
   * Support for the creation factory object.
   */
  trait Create {

    /**
     * Wraps any creation with the patching interface.
     *
     * @param create The creation to wrap.
     * @return The patching interface.
     */
    @inline
    implicit def createToCreatePatch(create: Difference.Create): CreatePatch =
    new CreatePatch(create)

  }

  /**
   * Support for the revision factory object.
   */
  trait Revise {

    /**
     * Wraps any revision with the patching interface.
     *
     * @param revise The revision to wrap.
     * @return The patching interface.
     */
    @inline
    implicit def reviseToRevisePatch(revise: Difference.Revise): RevisePatch =
    new RevisePatch(revise)

  }

  /**
   * Support for the deletion factory object.
   */
  trait Delete {

    /**
     * Wraps any deletion with the patching interface.
     *
     * @param delete The deletion to wrap.
     * @return The patching interface.
     */
    @inline
    implicit def deleteToDeletePatch(delete: Difference.Delete): DeletePatch =
    new DeletePatch(delete)

  }

  /**
   * The patching interface for creations.
   *
   * @param create The creation to provide the patching interface for.
   */
  final class CreatePatch(val create: Difference.Create) extends AnyVal {

    /**
     * Patches the difference by returning the created document.
     *
     * @return The created document.
     */
    def patch(): Document =
      create.document

  }

  /**
   * The patching interface for revisions.
   *
   * @param revise The revision to provide the patching interface for.
   */
  final class RevisePatch(val revise: Difference.Revise) extends AnyVal {

    /**
     * Applies this difference by applying all changes to the specified document.
     *
     * @param fromDocument The document to apply changes to.
     * @param hasher       The hasher to generate hashes with.
     * @return Either the resulting document after applying changes to the original document or any problems that were
     *         encountered.
     */
    def patch(fromDocument: Document)(implicit hasher: Hasher): Either[Vector[PatchingProblem], Document] = {
      implicit val ctx = Problem.Context(Vector(Value.String(fromDocument.title)))
      val hashCheck = Some(revise.fromHash) filter (_ != fromDocument.hash) map
        (PatchingProblem.HashMismatch(_, fromDocument.hash))
      revise.update.patch(fromDocument.content)
        .leftMap(e => hashCheck map (_ +: e.toList.toVector) getOrElse e.toList.toVector)
        .andThen(c => hashCheck map (p => invalid(Vector(p))) getOrElse valid(Document(revise.title, c)))
        .toEither
    }

  }

  /**
   * The patching interface for deletions.
   *
   * @param delete The deletion to provide the patching interface for.
   */
  final class DeletePatch(val delete: Difference.Delete) extends AnyVal {

    /**
     * Patches the difference by verifying the deleted document.
     *
     * @param document The document to verify the deletion of.
     * @param hasher   The hasher to generate hashes with.
     * @return True if the deleted document was verified, false otherwise.
     */
    def patch(document: Document)(implicit hasher: Hasher): Boolean =
      document.hash == delete.fromHash

  }

}

/**
 * Provides support for patching fragments with changes.
 */
object PatchingChanges {

  import Problem.Context
  import PatchingProblem.Attempt

  /**
   * Support for the addition factory object.
   */
  trait Add {

    /**
     * Wraps any addition with the patching interface.
     *
     * @param add The addition to wrap.
     * @return The patching interface.
     */
    @inline
    implicit def addToAddPatch(add: Change.Add): AddPatch =
    new AddPatch(add)

  }

  /**
   * Support for the removal factory object.
   */
  trait Remove {

    /**
     * Wraps any removal with the patching interface.
     *
     * @param remove The removal to wrap.
     * @return The patching interface.
     */
    @inline
    implicit def removeToRemovePatch(remove: Change.Remove): RemovePatch =
    new RemovePatch(remove)

  }

  /**
   * Support for the update factory object.
   */
  trait Update {

    /**
     * Wraps any update with the patching interface.
     *
     * @param update The update to wrap.
     * @return The patching interface.
     */
    @inline
    implicit def updateToUpdatePatch(update: Change.Update): UpdatePatch =
    new UpdatePatch(update)

  }

  /**
   * Support for the copies factory object.
   */
  trait Copy {

    /**
     * Wraps any copy with the patching interface.
     *
     * @param copy The copy to wrap.
     * @return The patching interface.
     */
    @inline
    implicit def copyToCopyPatch(copy: Change.Copy): CopyPatch =
    new CopyPatch(copy)

  }

  /**
   * Support for the replacement factory object.
   */
  trait Replace {

    /**
     * Wraps any replacement with the patching interface.
     *
     * @param replace The replacement to wrap.
     * @return The patching interface.
     */
    @inline
    implicit def replaceToReplacePatch(replace: Change.Replace): ReplacePatch =
    new ReplacePatch(replace)

  }

  /**
   * Support for the modification factory object.
   */
  trait Modify {

    /**
     * Wraps any modification with the patching interface.
     *
     * @param modify The modification to wrap.
     * @return The patching interface.
     */
    @inline
    implicit def modifyToModifyPatch(modify: Change.Modify): ModifyPatch =
    new ModifyPatch(modify)

  }

  /**
   * The patching interface for additions.
   *
   * @param add The addition to provide the patching interface for.
   */
  final class AddPatch(val add: Change.Add) extends AnyVal {

    /**
     * Applies the change by returning the fragment to add.
     *
     * @return The fragment to add or problems that were encountered adding the fragment.
     */
    def patch(): Attempt[Fragment] =
      valid(add.toFragment)

  }

  /**
   * The patching interface for removals.
   *
   * @param remove The removal to provide the patching interface for.
   */
  final class RemovePatch(val remove: Change.Remove) extends AnyVal {

    /**
     * Applies this change by verifying the removed fragment.
     *
     * @param fromFragment The fragment to verify the removal of.
     * @param hasher       The hasher to generate hashes with.
     * @param ctx          The context of this change application.
     * @return The problems that were encountered removing the fragment, if any.
     */
    def patch(fromFragment: Fragment)(implicit hasher: Hasher, ctx: Context): Attempt[Unit] =
      if (fromFragment.hash == remove.fromHash) valid(()) else
        invalid(PatchingProblem.List.of(PatchingProblem.HashMismatch(remove.fromHash, fromFragment.hash)))

  }

  /**
   * The patching interface for updates.
   *
   * @param update The update to provide the patching interface for.
   */
  final class UpdatePatch(val update: Change.Update) extends AnyVal {

    /**
     * Applies this change to the original fragment, creating the resulting fragment..
     *
     * @param fromFragment The fragment that is being updated.
     * @param hasher       The hasher to generate hashes with.
     * @param ctx          The context of this change application.
     * @return The resulting fragment or problems that were encountered updating the fragment.
     */
    def patch(fromFragment: Fragment)(implicit hasher: Hasher, ctx: Context): Attempt[Fragment] =
      update match {
        case c@Change.Copy(_) => c.patch(fromFragment)
        case r@Change.Replace(_, _) => r.patch(fromFragment)
        case m@Change.Modify(_, _) => m.patch(fromFragment)
      }

  }

  /**
   * The patching interface for copies.
   *
   * @param copy The copy to provide the patching interface for.
   */
  final class CopyPatch(val copy: Change.Copy) extends AnyVal {

    /** Applies this change by verifying and returning the fragment. */
    def patch(fromFragment: Fragment)(implicit hasher: Hasher, ctx: Context): Attempt[Fragment] =
      if (copy.theHash == fromFragment.hash) valid(fromFragment) else
        invalid(PatchingProblem.List.of(PatchingProblem.HashMismatch(copy.theHash, fromFragment.hash)))

  }

  /**
   * The patching interface for replacements.
   *
   * @param replace The replacement to provide the patching interface for.
   */
  final class ReplacePatch(val replace: Change.Replace) extends AnyVal {

    /** Applies this change by verifying the original fragment and returning the resulting fragment. */
    def patch(fromFragment: Fragment)(implicit hasher: Hasher, ctx: Context): Attempt[Fragment] =
      if (replace.fromHash == fromFragment.hash) valid(replace.toFragment) else
        invalid(PatchingProblem.List.of(PatchingProblem.HashMismatch(replace.fromHash, fromFragment.hash)))

  }

  /**
   * The patching interface for modifications.
   *
   * @param modify The modification to provide the patching interface for.
   */
  final class ModifyPatch(val modify: Change.Modify) extends AnyVal {

    /** Applies this change by verifying the original table, applying all changes and returning the resulting table. */
    def patch(fromFragment: Fragment)(implicit hasher: Hasher, ctx: Context): Attempt[Fragment] = {
      // Verify that the from table's hash matches.
      val hashCheck: Attempt[Vector[(Value, Fragment)]] =
        if (modify.fromHash == fromFragment.hash) valid(Vector())
        else invalid(PatchingProblem.List.of(PatchingProblem.HashMismatch(modify.fromHash, fromFragment.hash)))
      (fromFragment match {
        case v@Value() =>
          NonEmptyList(hashCheck, List(invalid(PatchingProblem.List.of(PatchingProblem.TypeMismatch(v))))).reduce
        case fromTable@Table(_) =>
          // Verify that all from keys have matching changes.
          val keyCheck = if ((fromTable.keys -- modify.changes.keys).isEmpty) valid(Vector()) else
            invalid(PatchingProblem.List.of(PatchingProblem.MissingChangeKeys(fromTable.keys -- modify.changes.keys)))
          // Apply all the changes.
          val _ctx = ctx
          val entries = modify.changes map { case (k, c) =>
            implicit val ctx: Context = _ctx.push(k)
            c match {
              case Change.Add(_) if fromTable.get(k).nonEmpty => invalid(PatchingProblem.List.of(PatchingProblem.UnexpectedEntry(k)))
              case add@Change.Add(_) => add.patch() map (v => Vector(k -> v))
              case _ if fromTable.get(k).isEmpty => invalid(PatchingProblem.List.of(PatchingProblem.MissingEntry(k)))
              case remove@Change.Remove(_) => remove.patch(fromTable(k)) map (_ => Vector())
              case copy@Change.Copy(_) => copy.patch(fromTable(k)) map (v => Vector(k -> v))
              case replace@Change.Replace(_, _) => replace.patch(fromTable(k)) map (v => Vector(k -> v))
              case modify@Change.Modify(_, _) => modify.patch(fromTable(k)) map (v => Vector(k -> v))
            }
          }
          // Construct the resulting table.
          NonEmptyList(hashCheck, keyCheck :: entries.toList).reduce
      }) map (e => Table(e: _*))
    }

  }

}

/**
 * Base class for problems that occur while patching differences or changes.
 */
sealed trait PatchingProblem extends Problem

/**
 * Concrete problem implementations.
 */
object PatchingProblem extends Problem.Factory[PatchingProblem] {

  /**
   * Problem returned when hashes that are expected to match do not.
   *
   * @param expected The value of the expected hash.
   * @param found    The value of the hash that was encountered.
   * @param context  The context where the problem occurred.
   */
  case class HashMismatch(expected: Hash, found: Hash)(implicit val context: Context) extends PatchingProblem

  /**
   * Problem returned when keys in a table have no matching keys in a modify.
   *
   * @param missing The keys in the table that did not have matching keys in a modify.
   * @param context The context where the problem occurred.
   */
  case class MissingChangeKeys(missing: SortedSet[Value])(implicit val context: Context) extends PatchingProblem

  /**
   * Problem returned when a key in a modify has no matching key in a table.
   *
   * @param expected The key in the modify that did not have a matching key in a table.
   * @param context  The context where the problem occurred.
   */
  case class MissingEntry(expected: Value)(implicit val context: Context) extends PatchingProblem

  /**
   * Problem returned when a key in a table was not expected to be there.
   *
   * @param found   The key in the table that was not expected to be there.
   * @param context The context where the problem occurred.
   */
  case class UnexpectedEntry(found: Value)(implicit val context: Context) extends PatchingProblem

  /**
   * Problem returned when a fragment expected to be a table is a value instead.
   *
   * @param found   The value that was found where a table was expected.
   * @param context The context where the problem occurred.
   */
  case class TypeMismatch(found: Value)(implicit val context: Context) extends PatchingProblem

}
