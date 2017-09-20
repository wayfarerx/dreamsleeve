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

import collection.immutable.SortedMap

import cats.implicits._

/**
 * Base class for all differences between documents.
 */
sealed abstract class Difference extends Data

/**
 * Implementations of differences.
 */
object Difference {

  /**
   * Extracts any difference implementation.
   *
   * @param difference The difference to extract.
   * @return True for every difference.
   */
  def unapply(difference: Difference): Boolean =
    true

  /**
   * Represents when a document is first created.
   *
   * @param document The document to create.
   */
  case class Create(document: Document) extends Difference {

    /* Test for equality with this create. */
    override protected[data] def calculateEquals(that: Any): EqualsOperation[Boolean] = for {
      c <- EqualsTask.ofType[Create](that)
      d <- document.calculateEquals(c.document)
    } yield d

    /* Calculate the string for this create. */
    override protected[data] def calculateToString(): ToStringOperation[Unit] = for {
      _ <- ToStringTask.begin("Create")
      _ <- document.calculateToString()
      _ <- ToStringTask.end()
    } yield ()

    /* Calculate the hash for this create. */
    override protected def calculateHash(): HashOperation[Hash] = for {
      d <- document.hashOperation
      h <- HashTask.hash(Create.Header, d)
    } yield h

  }

  /**
   * Declarations associated with creates.
   */
  object Create extends
    patching_data.PatchingCreate {

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
  case class Revise(fromHash: Hash, title: String, update: Update) extends Difference {

    /* Test for equality with this revise. */
    override protected[data] def calculateEquals(that: Any): EqualsOperation[Boolean] = for {
      r <- EqualsTask.ofType[Revise](that)
      f <- EqualsTask.areEqual(fromHash, r.fromHash)
      t <- EqualsTask.areEqual(title, r.title)
      d <- update.calculateEquals(r.update)
    } yield f && t && d

    /* Calculate the string for this revise. */
    override protected[data] def calculateToString(): ToStringOperation[Unit] = for {
      _ <- ToStringTask.begin("Revise")
      _ <- ToStringTask.emit(fromHash)
      _ <- ToStringTask.emit(title)
      _ <- update.calculateToString()
      _ <- ToStringTask.end()
    } yield ()

    /* Calculate the hash for this revise. */
    override protected def calculateHash(): HashOperation[Hash] = for {
      u <- update.hashOperation
      h <- HashTask.hash(Revise.Header, fromHash, title, u)
    } yield h

  }

  /**
   * Factory for revises.
   */
  object Revise extends
    diff_data.DiffRevise with
    patching_data.PatchingRevise {

    /** The header for revises. */
    val Header: Byte = 0x87.toByte


    /**
     * Creates a revise for the specified original document, resulting title and content update.
     *
     * @param from   The the original document.
     * @param title  The title of the resulting document.
     * @param update The change to apply to the original document's content.
     */
    def apply(from: Document, title: String, update: Update): Revise =
      Revise(from.hash, title, update)

  }

  /**
   * Represents when a document is finally deleted.
   *
   * @param fromHash The hash of the document to delete.
   */
  case class Delete(fromHash: Hash) extends Difference {

    /* Test for equality with this delete. */
    override protected[data] def calculateEquals(that: Any): EqualsOperation[Boolean] = for {
      d <- EqualsTask.ofType[Delete](that)
      f <- EqualsTask.areEqual(fromHash, d.fromHash)
    } yield f

    /* Calculate the string for this delete. */
    override protected[data] def calculateToString(): ToStringOperation[Unit] = for {
      _ <- ToStringTask.begin("Delete")
      _ <- ToStringTask.emit(fromHash)
      _ <- ToStringTask.end()
    } yield ()


    /* Calculate the hash for this delete. */
    override protected def calculateHash(): HashOperation[Hash] = for {
      h <- HashTask.hash(Delete.Header, fromHash)
    } yield h

  }

  /**
   * Factory for deletes.
   */
  object Delete extends
    patching_data.PatchingDelete {

    /** The header for deletes. */
    val Header: Byte = 0x78.toByte

    /**
     * Creates a delete for the specified document.
     *
     * @param document The document to create the delete for.
     * @return A delete for the specified document.
     */
    def apply(document: Document): Delete =
      Delete(document.hash)

  }

}

/**
 * Base type for all changes applied to fragments in a table or document.
 */
sealed abstract class Change extends Data

/**
 * Implementations of changes.
 */
object Change {

  /**
   * Extracts any change implementation.
   *
   * @param change The change to extract.
   * @return True for every change.
   */
  def unapply(change: Change): Boolean =
    true

  /**
   * Add a fragment into a table where there was none before.
   *
   * @param toFragment The fragment to add into a table.
   */
  case class Add(toFragment: Fragment) extends Change {

    /* Test for equality with this add. */
    override protected[data] def calculateEquals(that: Any): EqualsOperation[Boolean] = for {
      a <- EqualsTask.ofType[Add](that)
      t <- toFragment.calculateEquals(a.toFragment)
    } yield t

    /* Calculate the string for this add. */
    override protected[data] def calculateToString(): ToStringOperation[Unit] = for {
      _ <- ToStringTask.begin("Add")
      _ <- toFragment.calculateToString()
      _ <- ToStringTask.end()
    } yield ()

    /* Calculate the hash for this add. */
    override protected def calculateHash(): HashOperation[Hash] = for {
      t <- toFragment.hashOperation
      h <- HashTask.hash(Add.Header, t)
    } yield h

  }

  /**
   * Declarations associated with adds.
   */
  object Add extends
    patching_data.PatchingAdd {

    /** The header for adds. */
    val Header: Byte = 0x69.toByte

  }

  /**
   * Remove a fragment from a table.
   *
   * @param fromHash The hash of the fragment to remove from a table.
   */
  case class Remove(fromHash: Hash) extends Change {

    /* Test for equality with this remove. */
    override protected[data] def calculateEquals(that: Any): EqualsOperation[Boolean] = for {
      r <- EqualsTask.ofType[Remove](that)
      f <- EqualsTask.areEqual(fromHash, r.fromHash)
    } yield f

    /* Calculate the string for this remove. */
    override protected[data] def calculateToString(): ToStringOperation[Unit] = for {
      _ <- ToStringTask.begin("Remove")
      _ <- ToStringTask.emit(fromHash)
      _ <- ToStringTask.end()
    } yield ()

    /* Calculate the hash for this remove. */
    override protected def calculateHash(): HashOperation[Hash] = for {
      h <- HashTask.hash(Remove.Header, fromHash)
    } yield h

  }

  /**
   * Factory for removes.
   */
  object Remove extends
    patching_data.PatchingRemove {

    /** The header for removes. */
    val Header: Byte = 0x5A.toByte

    /**
     * Creates a remove for the specified fragment.
     *
     * @param fromFragment The fragment that is being removed.
     * @return A remove for the specified fragment.
     */
    def apply(fromFragment: Fragment): Remove =
      Remove(fromFragment.hash)

  }

}

/**
 * Base type for all changes that update existing fragments.
 */
sealed abstract class Update extends Change

/**
 * Factory for updates.
 */
object Update extends
  diff_data.DiffUpdate with
  patching_data.PatchingUpdate {

  /**
   * Extracts any update implementation.
   *
   * @param update The update to extract.
   * @return True for every update.
   */
  def unapply(update: Update): Boolean =
    true

  /**
   * Copy an existing fragment in a table or document.
   *
   * @param theHash The hash of both fragments.
   */
  case class Copy(theHash: Hash) extends Update {

    /* Test for equality with this copy. */
    override protected[data] def calculateEquals(that: Any): EqualsOperation[Boolean] = for {
      c <- EqualsTask.ofType[Copy](that)
      t <- EqualsTask.areEqual(theHash, c.theHash)
    } yield t

    /* Calculate the string for this copy. */
    override protected[data] def calculateToString(): ToStringOperation[Unit] = for {
      _ <- ToStringTask.begin("Copy")
      _ <- ToStringTask.emit(theHash)
      _ <- ToStringTask.end()
    } yield ()

    /* Generate the theHash for this copy. */
    override protected def calculateHash(): HashOperation[Hash] = for {
      h <- HashTask.hash(Copy.Header, theHash)
    } yield h

  }

  /**
   * Factory for copies.
   */
  object Copy extends
    patching_data.PatchingCopy {

    /** The header for copies. */
    val Header: Byte = 0x4B.toByte

    /**
     * Creates a copy for the specified fragment.
     *
     * @param fragment The fragment that is being copied.
     * @return A copy for the specified fragment.
     */
    def apply(fragment: Fragment): Copy =
      Copy(fragment.hash)

  }

  /**
   * Replace an existing fragment in a table or document.
   *
   * @param fromHash   The hash of the original fragment.
   * @param toFragment The fragment to replace the original fragment with.
   */
  case class Replace(fromHash: Hash, toFragment: Fragment) extends Update {

    /* Test for equality with this replace. */
    override protected[data] def calculateEquals(that: Any): EqualsOperation[Boolean] = for {
      r <- EqualsTask.ofType[Replace](that)
      f <- EqualsTask.areEqual(fromHash, r.fromHash)
      t <- toFragment.calculateEquals(r.toFragment)
    } yield f && t

    /* Calculate the string for this copy. */
    override protected[data] def calculateToString(): ToStringOperation[Unit] = for {
      _ <- ToStringTask.begin("Replace")
      _ <- ToStringTask.emit(fromHash)
      _ <- toFragment.calculateToString()
      _ <- ToStringTask.end()
    } yield ()

    /* Calculate the hash for this replace. */
    override protected def calculateHash(): HashOperation[Hash] = for {
      t <- toFragment.hashOperation
      h <- HashTask.hash(Replace.Header, fromHash, t)
    } yield h

  }

  /**
   * Factory for replaces.
   */
  object Replace extends
    patching_data.PatchingReplace {

    /** The header for replaces. */
    val Header: Byte = 0x3C.toByte

    /**
     * Creates a replace for the specified fragments.
     *
     * @param from The fragment that is being replaced
     * @param to   The fragment that is doing the replacing.
     * @return A replace for the specified fragments.
     */
    def apply(from: Fragment, to: Fragment): Replace =
      Replace(from.hash, to)

  }

  /**
   * Modify an existing table inside a table or document.
   *
   * @param fromHash The hash of the original table.
   * @param changes  The changes to be applied to the table.
   */
  case class Modify(fromHash: Hash, changes: SortedMap[Value, Change]) extends Update {

    /* Test for equality with this modify. */
    override protected[data] def calculateEquals(that: Any): EqualsOperation[Boolean] = for {
      m <- EqualsTask.ofType[Modify](that)
      f <- EqualsTask.areEqual(fromHash, m.fromHash)
      e <- (EqualsTask.areEqual(changes.size, m.changes.size) /: changes.zip(m.changes)) { (r, e) =>
        for {
          rr <- r
          ((k1, v1), (k2, v2)) = e
          kk <- k1.calculateEquals(k2)
          vv <- v1.calculateEquals(v2)
        } yield rr && kk && vv
      }
    } yield f && e

    /* Calculate the string for this modify. */
    override protected[data] def calculateToString(): ToStringOperation[Unit] = for {
      _ <- ToStringTask.begin("Modify")
      _ <- ToStringTask.emit(fromHash)
      _ <- ToStringTask.beginMap()
      _ <- (ToStringTask.pure() /: changes) { (r, e) =>
        for {
          _ <- r
          (k, v) = e
          _ <- ToStringTask.beginEntry()
          _ <- k.calculateToString()
          _ <- v.calculateToString()
          _ <- ToStringTask.endEntry()
        } yield ()
      }
      _ <- ToStringTask.endMap()
      _ <- ToStringTask.end()
    } yield ()

    /* Calculate the hash for this modify. */
    override protected def calculateHash(): HashOperation[Hash] = for {
      e <- (HashTask.pure(Vector[Hash]()) /: changes) { (r, c) =>
        for {
          rr <- r
          (k, v) = c
          kk <- k.hashOperation
          vv <- v.hashOperation
        } yield rr ++ Vector(kk, vv)
      }
      h <- HashTask.hash(Modify.Header, fromHash, e)
    } yield h

  }

  /**
   * Factory for modifies.
   */
  object Modify extends
    patching_data.PatchingModify {

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
     * @return A new modify for the specified table and changes.
     */
    def apply(from: Table, changes: (Value, Change)*): Modify =
      apply(from.hash, changes: _*)

  }


}