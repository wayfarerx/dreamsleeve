/*
 * differences.scala
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
import cats.implicits._
import Validated.{invalid, valid}

import scala.collection.immutable.{SortedMap, SortedSet}

/**
 * Base class for all differences between documents.
 */
sealed trait Difference extends Hashable

/**
 * Implementations of differences.
 */
object Difference {

  /**
   * Represents when a document is first created.
   *
   * @param document The document to create.
   */
  case class Create(document: Document) extends Difference {

    /**
     * Applies this difference by returning the created document.
     *
     * @return The created document.
     */
    def apply(): Document =
      document

    /* Generate the hash for this create. */
    override private[data] def generateHash(implicit hasher: Hasher): Hash =
      hasher.hashCreate(document.hash)

  }

  /**
   * Declarations associated with creates.
   */
  object Create {

    /** The header for creates. */
    val Header: Byte = 0x96.toByte

  }

  /**
   * Represents when an existing document is revised.
   *
   * @param fromHash The hash of the original document.
   * @param title    The title of the resulting document.
   * @param update   The change to apply to the original document's content.
   */
  case class Revise(fromHash: Hash, title: String, update: Change.Update) extends Difference {

    /**
     * Applies this difference by applying all changes to the specified document.
     *
     * @param fromDocument The document to apply changes to.
     * @param hasher       The hasher to generate hashes with.
     * @return Either the resulting document after applying changes to the original document or any problems that were
     *         encountered.
     */
    def apply(fromDocument: Document)(implicit hasher: Hasher): Either[Vector[Problem], Document] = {
      implicit val ctx = Problem.Context(Vector(Value.String(fromDocument.title)))
      val hashCheck = Some(fromHash) filter (_ != fromDocument.hash) map (Problem.HashMismatch(_, fromDocument.hash))
      update(fromDocument.content)
        .leftMap(e => hashCheck map (_ +: e.toList.toVector) getOrElse e.toList.toVector)
        .andThen(c => hashCheck map (p => invalid(Vector(p))) getOrElse valid(Document(title, c)))
        .toEither
    }

    /* Generate the hash for this revise. */
    override private[data] def generateHash(implicit hasher: Hasher): Hash =
      hasher.hashRevise(fromHash, title, update.hash)

  }

  /**
   * Factory for revises.
   */
  object Revise {

    /** The header for revises. */
    val Header: Byte = 0x87.toByte


    /**
     * Creates a revise for the specified original document, resulting title and content update.
     *
     * @param from   The the original document.
     * @param title  The title of the resulting document.
     * @param update The change to apply to the original document's content.
     * @param hasher The hasher to generate hashes with.
     */
    def apply(from: Document, title: String, update: Change.Update)(implicit hasher: Hasher): Revise =
      Revise(from.hash, title, update)

    /**
     * Creates a revise by collecting the differences between two documents.
     *
     * @param from   The document that is being revised.
     * @param to     The document that is a result of the revision.
     * @param hasher The hasher to generate hashes with.
     * @return A revise collecting the differences between two documents.
     */
    def apply(from: Document, to: Document)(implicit hasher: Hasher): Revise =
      Revise(from, to.title, Change.Update(from.content, to.content))

  }

  /**
   * Represents when a document is finally deleted.
   *
   * @param fromHash The hash of the document to delete.
   */
  case class Delete(fromHash: Hash) extends Difference {

    /**
     * Applies this difference by verifying the deleted document.
     *
     * @param document The document to verify the deletion of.
     * @param hasher   The hasher to generate hashes with.
     * @return True if the deleted document was verified, false otherwise.
     */
    def apply(document: Document)(implicit hasher: Hasher): Boolean =
      document.hash == fromHash


    /* Generate the hash for this delete. */
    override private[data] def generateHash(implicit hasher: Hasher): Hash =
      hasher.hashDelete(fromHash)

  }

  /**
   * Factory for deletes.
   */
  object Delete {

    /** The header for deletes. */
    val Header: Byte = 0x78.toByte

    /**
     * Creates a delete for the specified document.
     *
     * @param document The document to create the delete for.
     * @param hasher   The hasher to generate hashes with.
     * @return A delete for the specified document.
     */
    def apply(document: Document)(implicit hasher: Hasher): Delete =
      Delete(document.hash)

  }

}

/**
 * Base type for all changes applied to fragments in a table or document.
 */
sealed trait Change extends Hashable

/**
 * Implementations of changes.
 */
object Change {

  import Problem.Context

  /** The type that represents the result error-prone operations on difference objects. */
  private[data] type Attempt[T] = Validated[Problem.List, T]

  /**
   * Add a fragment into a table where there was none before.
   *
   * @param toFragment The fragment to add into a table.
   */
  case class Add(toFragment: Fragment) extends Change {

    /**
     * Applies this change by returning the fragment to add.
     *
     * @return The fragment to add or problems that were encountered adding the fragment.
     */
    private[data] def apply(): Attempt[Fragment] =
      valid(toFragment)

    /* Generate the hash for this add. */
    override private[data] def generateHash(implicit hasher: Hasher): Hash =
      hasher.hashAdd(toFragment.hash)

  }

  /**
   * Declarations associated with adds.
   */
  object Add {

    /** The header for adds. */
    val Header: Byte = 0x69.toByte

  }

  /**
   * Remove a fragment from a table.
   *
   * @param fromHash The hash of the fragment to remove from a table.
   */
  case class Remove(fromHash: Hash) extends Change {

    /**
     * Applies this change by verifying the removed fragment.
     *
     * @param fromFragment The fragment to verify the removal of.
     * @param hasher       The hasher to generate hashes with.
     * @param ctx          The context of this change application.
     * @return The problems that were encountered removing the fragment, if any.
     */
    private[data] def apply(fromFragment: Fragment)(implicit hasher: Hasher, ctx: Context): Attempt[Unit] =
      if (fromFragment.hash == fromHash) valid(()) else
        invalid(Problem.List.of(Problem.HashMismatch(fromHash, fromFragment.hash)))

    /* Generate the hash for this remove. */
    override private[data] def generateHash(implicit hasher: Hasher): Hash =
      hasher.hashRemove(fromHash)

  }

  /**
   * Factory for removes.
   */
  object Remove {

    /** The header for removes. */
    val Header: Byte = 0x5A.toByte

    /**
     * Creates a remove for the specified fragment.
     *
     * @param fromFragment The fragment that is being removed.
     * @param hasher       The hasher to generate hashes with.
     * @return A remove for the specified fragment.
     */
    def apply(fromFragment: Fragment)(implicit hasher: Hasher): Remove =
      Remove(fromFragment.hash)

  }

  /**
   * Base type for all changes that update existing fragments.
   */
  sealed trait Update extends Change {

    /**
     * Applies this change to the original fragment, creating the resulting fragment..
     *
     * @param fromFragment The fragment that is being updated.
     * @param hasher       The hasher to generate hashes with.
     * @param ctx          The context of this change application.
     * @return The resulting fragment or problems that were encountered updating the fragment.
     */
    private[data] def apply(fromFragment: Fragment)(implicit hasher: Hasher, ctx: Context): Attempt[Fragment]

  }

  /**
   * Factory for updates.
   */
  object Update {

    /**
     * Creates an update by collecting the differences between two fragments.
     *
     * @param from   The fragment that is being updated.
     * @param to     The fragment that is a result of the update.
     * @param hasher The hasher to generate hashes with.
     * @return An update collecting the differences between two fragments.
     */
    def apply(from: Fragment, to: Fragment)(implicit hasher: Hasher): Update = (from, to) match {
      case (f, t) if f == t => Change.Copy(f)
      case ((Value(), _) | (_, Value())) => Change.Replace(from, to)
      case (fromTable@Table(_), toTable@Table(_)) => Modify(fromTable, (
        (toTable.keys -- fromTable.keys map (k => k -> Change.Add(toTable(k)))) ++
          fromTable.keys.map { k =>
            (k, toTable get k map (apply(fromTable(k), _)) getOrElse Change.Remove(fromTable(k)))
          }
        ).toSeq: _*)
    }

  }

  /**
   * Copy an existing fragment in a table or document.
   *
   * @param theHash The hash of both fragments.
   */
  case class Copy(theHash: Hash) extends Update {

    /* Apply this change by verifying and returning the fragment. */
    override private[data] def apply(fromFragment: Fragment)(implicit hasher: Hasher, ctx: Context): Attempt[Fragment] =
      if (theHash == fromFragment.hash) valid(fromFragment) else
        invalid(Problem.List.of(Problem.HashMismatch(theHash, fromFragment.hash)))

    /* Generate the theHash for this copy. */
    override private[data] def generateHash(implicit theHasher: Hasher): Hash =
      theHasher.hashCopy(theHash)

  }

  /**
   * Factory for copies.
   */
  object Copy {

    /** The header for copies. */
    val Header: Byte = 0x4B.toByte

    /**
     * Creates a copy for the specified fragment.
     *
     * @param fragment The fragment that is being copied.
     * @param hasher   The hasher to generate hashes with.
     * @return A copy for the specified fragment.
     */
    def apply(fragment: Fragment)(implicit hasher: Hasher): Copy =
      Copy(fragment.hash)

  }

  /**
   * Replace an existing fragment in a table or document.
   *
   * @param fromHash   The hash of the original fragment.
   * @param toFragment The fragment to replace the original fragment with.
   */
  case class Replace(fromHash: Hash, toFragment: Fragment) extends Update {

    /* Apply this change by verifying the original fragment and returning the resulting fragment. */
    override private[data] def apply(fromFragment: Fragment)(implicit hasher: Hasher, ctx: Context): Attempt[Fragment] =
      if (fromHash == fromFragment.hash) valid(toFragment) else
        invalid(Problem.List.of(Problem.HashMismatch(fromHash, fromFragment.hash)))

    /* Generate the hash for this replace. */
    override private[data] def generateHash(implicit hasher: Hasher): Hash =
      hasher.hashReplace(fromHash, toFragment.hash)

  }

  /**
   * Factory for replaces.
   */
  object Replace {

    /** The header for replaces. */
    val Header: Byte = 0x3C.toByte

    /**
     * Creates a replace for the specified fragments.
     *
     * @param from   The fragment that is being replaced
     * @param to     The fragment that is doing the replacing.
     * @param hasher The hasher to generate hashes with.
     * @return A replace for the specified fragments.
     */
    def apply(from: Fragment, to: Fragment)(implicit hasher: Hasher): Replace =
      Replace(from.hash, to)

  }

  /**
   * Modify an existing table inside a table or document.
   *
   * @param fromHash The hash of the original table.
   * @param changes  The changes to be applied to the table.
   */
  case class Modify(fromHash: Hash, changes: SortedMap[Value, Change]) extends Update {

    /* Apply this change by verifying the original table, applying all changes and returning the resulting table. */
    override private[data] def apply(fromFragment: Fragment)(implicit hasher: Hasher, ctx: Context): Attempt[Fragment] = {
      // Verify that the from table's hash matches.
      val hashCheck: Attempt[Vector[(Value, Fragment)]] = if (fromHash == fromFragment.hash) valid(Vector()) else
        invalid(Problem.List.of(Problem.HashMismatch(fromHash, fromFragment.hash)))
      (fromFragment match {
        case v@Value() =>
          NonEmptyList(hashCheck, List(invalid(Problem.List.of(Problem.TypeMismatch(v))))).reduce
        case fromTable@Table(_) =>
          // Verify that all from keys have matching changes.
          val keyCheck = if ((fromTable.keys -- changes.keys).isEmpty) valid(Vector()) else
            invalid(Problem.List.of(Problem.MissingChangeKeys(fromTable.keys -- changes.keys)))
          // Apply all the changes.
          val _ctx = ctx
          val entries = changes map { case (k, c) =>
            implicit val ctx: Context = _ctx.push(k)
            c match {
              case Add(_) if fromTable.get(k).nonEmpty => invalid(Problem.List.of(Problem.UnexpectedEntry(k)))
              case add@Add(_) => add() map (v => Vector(k -> v))
              case _ if fromTable.get(k).isEmpty => invalid(Problem.List.of(Problem.MissingEntry(k)))
              case remove@Remove(_) => remove(fromTable(k)) map (_ => Vector())
              case copy@Copy(_) => copy(fromTable(k)) map (v => Vector(k -> v))
              case replace@Replace(_, _) => replace(fromTable(k)) map (v => Vector(k -> v))
              case modify@Modify(_, _) => modify(fromTable(k)) map (v => Vector(k -> v))
            }
          }
          // Construct the resulting table.
          NonEmptyList(hashCheck, keyCheck :: entries.toList).reduce
      }) map (e => Table(e: _*))
    }

    /* Generate the hash for this modify. */
    override private[data] def generateHash(implicit hasher: Hasher): Hash =
      hasher.hashModify(fromHash, changes flatMap { case (k, v) => Seq(k.hash, v.hash) })

  }

  /**
   * Factory for modifies.
   */
  object Modify {

    /** The header for modifies. */
    val Header: Byte = 0x2D.toByte

    /**
     * Creates a new modify.
     *
     * @param fromHash The hash of the original table.
     * @param changes  The changes to be applied to the table.
     * @return A new modify.
     */
    def apply(fromHash: Hash, changes: (Value, Change)*): Modify =
      Modify(fromHash, SortedMap(changes: _*))


    /**
     * Creates a new modify for the specified table and changes.
     *
     * @param from    The original table.
     * @param changes The changes to be applied to the table.
     * @param hasher  The hasher to generate hashes with.
     * @return A new modify for the specified table and changes.
     */
    def apply(from: Table, changes: (Value, Change)*)(implicit hasher: Hasher): Modify =
      apply(from.hash, changes: _*)

  }

}

/**
 * Base class for problems that occur while processing differences.
 */
sealed trait Problem {

  /** The context where this problem occurred. */
  def context: Problem.Context

  /** The message that describes this problem. */
  def message: String

}

/**
 * Concrete problem implementations.
 */
object Problem {

  /** The type of a list of problems. */
  private[data] type List = NonEmptyList[Problem]

  /** The factory for lists of problems. */
  private[data] val List = NonEmptyList

  /**
   * The context passed among change applications.
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

  /**
   * Problem returned when hashes that are expected to match do not.
   *
   * @param expected The value of the expected hash.
   * @param found    The value of the hash that was encountered.
   * @param context  The context where the problem occurred.
   */
  case class HashMismatch(expected: Hash, found: Hash)(implicit val context: Context) extends Problem {
    override lazy val message: String =
      s"Hash mismatch at /${context.location mkString "/"}" +
        s" expected ${expected.toPrefixString()} found ${found.toPrefixString()}."
  }

  /**
   * Problem returned when keys in a table have no matching keys in a modify.
   *
   * @param missing The keys in the table that did not have matching keys in a modify.
   * @param context The context where the problem occurred.
   */
  case class MissingChangeKeys(missing: SortedSet[Value])(implicit val context: Context) extends Problem {
    override lazy val message: String =
      s"Missing change keys at /${context.location mkString "/"} expected ${missing mkString ","}."
  }

  /**
   * Problem returned when a key in a modify has no matching key in a table.
   *
   * @param expected The key in the modify that did not have a matching key in a table.
   * @param context  The context where the problem occurred.
   */
  case class MissingEntry(expected: Value)(implicit val context: Context) extends Problem {
    override lazy val message: String =
      s"Missing entry at ${context.location mkString "/"} expected $expected found nothing."
  }

  /**
   * Problem returned when a key in a table was not expected to be there.
   *
   * @param found   The key in the table that was not expected to be there.
   * @param context The context where the problem occurred.
   */
  case class UnexpectedEntry(found: Value)(implicit val context: Context) extends Problem {
    override lazy val message: String =
      s"Unexpected entry at ${context.location mkString "/"} expected nothing found $found."
  }

  /**
   * Problem returned when a fragment expected to be a table is a value instead.
   *
   * @param found   The value that was found where a table was expected.
   * @param context The context where the problem occurred.
   */
  case class TypeMismatch(found: Value)(implicit val context: Context) extends Problem {
    override lazy val message: String =
      s"Type mismatch at ${context.location mkString "/"} expected a table found $found."
  }

}
