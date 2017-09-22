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

import cats.Eval

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
   * @return Eval.now(true) for every difference.
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
    override protected[data] def calculateEquals(that: Any): Eval[Boolean] = that match {
      case Create(thatDocument) => document.calculateEquals(thatDocument)
      case _ => Eval.False
    }

    /* Calculate the hash for this create. */
    override protected def calculateHash(generator: Hash.Generator): Eval[Hash] =
      for (d <- document.evalHash(generator)) yield generator.hash(Create.Header, d)

    /* Calculate the string for this create. */
    override protected[data] def calculateToString(): Eval[String] =
      for (d <- document.calculateToString()) yield s"Create($d)"

  }

  /**
   * Declarations associated with creates.
   */
  object Create extends
    patch_data.Creates {

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
    override protected[data] def calculateEquals(that: Any): Eval[Boolean] = that match {
      case Revise(thatFromHash, thatTitle, thatUpdate) if thatFromHash == fromHash && thatTitle == title =>
        update.calculateEquals(thatUpdate)
      case _ => Eval.False
    }

    /* Calculate the hash for this revise. */
    override protected def calculateHash(generator: Hash.Generator): Eval[Hash] =
      for (u <- update.evalHash(generator)) yield generator.hash(Revise.Header, fromHash, title, u)

    /* Calculate the string for this revise. */
    override protected[data] def calculateToString(): Eval[String] =
      for (u <- update.calculateToString()) yield s"Revise($fromHash,$title,$u)"

  }

  /**
   * Factory for revises.
   */
  object Revise extends
    diff_data.Revisions with
    patch_data.Revises {

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
    override protected[data] def calculateEquals(that: Any): Eval[Boolean] = that match {
      case Delete(thatFromHash) if thatFromHash == fromHash => Eval.True
      case _ => Eval.False
    }

    /* Calculate the hash for this delete. */
    override protected def calculateHash(generator: Hash.Generator): Eval[Hash] =
      Eval.now(generator.hash(Delete.Header, fromHash))

    /* Calculate the string for this delete. */
    override protected[data] def calculateToString(): Eval[String] =
      Eval.now(s"Delete($fromHash)")

  }

  /**
   * Factory for deletes.
   */
  object Delete extends
    patch_data.Deletes {

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
   * @return Eval.now(true) for every change.
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
    override protected[data] def calculateEquals(that: Any): Eval[Boolean] = that match {
      case Add(thatToFragment) => toFragment.calculateEquals(thatToFragment)
      case _ => Eval.False
    }

    /* Calculate the hash for this add. */
    override protected def calculateHash(generator: Hash.Generator): Eval[Hash] =
      for (t <- toFragment.evalHash(generator)) yield generator.hash(Add.Header, t)

    /* Calculate the string for this add. */
    override protected[data] def calculateToString(): Eval[String] =
      for (f <- toFragment.calculateToString()) yield s"Add($f)"

  }

  /**
   * Declarations associated with adds.
   */
  object Add extends
    patch_data.Adds {

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
    override protected[data] def calculateEquals(that: Any): Eval[Boolean] = that match {
      case Remove(thatFromHash) if thatFromHash == fromHash => Eval.True
      case _ => Eval.False
    }

    /* Calculate the hash for this remove. */
    override protected def calculateHash(generator: Hash.Generator): Eval[Hash] =
      Eval.now(generator.hash(Remove.Header, fromHash))

    /* Calculate the string for this remove. */
    override protected[data] def calculateToString(): Eval[String] =
      Eval.now(s"Remove($fromHash)")

  }

  /**
   * Factory for removes.
   */
  object Remove extends
    patch_data.Removes {

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
  diff_data.Updates with
  patch_data.Updates {

  /**
   * Extracts any update implementation.
   *
   * @param update The update to extract.
   * @return Eval.now(true) for every update.
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
    override protected[data] def calculateEquals(that: Any): Eval[Boolean] = that match {
      case Copy(thatTheHash) if thatTheHash == theHash => Eval.True
      case _ => Eval.False
    }

    /* Generate the theHash for this copy. */
    override protected def calculateHash(generator: Hash.Generator): Eval[Hash] =
      Eval.now(generator.hash(Copy.Header, theHash))

    /* Calculate the string for this copy. */
    override protected[data] def calculateToString(): Eval[String] =
      Eval.now(s"Copy($theHash)")

  }

  /**
   * Factory for copies.
   */
  object Copy extends
    patch_data.Copies {

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
    override protected[data] def calculateEquals(that: Any): Eval[Boolean] = that match {
      case Replace(thatFromHash, thatToFragment) if thatFromHash == fromHash =>
        toFragment.calculateEquals(thatToFragment)
      case _ => Eval.False
    }

    /* Calculate the hash for this replace. */
    override protected def calculateHash(generator: Hash.Generator): Eval[Hash] =
      for (t <- toFragment.evalHash(generator)) yield generator.hash(Replace.Header, fromHash, t)

    /* Calculate the string for this replace. */
    override protected[data] def calculateToString(): Eval[String] =
      for (f <- toFragment.calculateToString()) yield s"Replace($fromHash,$f)"

  }

  /**
   * Factory for replaces.
   */
  object Replace extends
    patch_data.Replaces {

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
    override protected[data] def calculateEquals(that: Any): Eval[Boolean] = that match {
      case Modify(thatFromHash, thatChanges) if thatFromHash == fromHash && thatChanges.size == changes.size =>
        (Eval.True /: changes.flatMap(e => Seq(e._1, e._2)).zip(thatChanges.flatMap(e => Seq(e._1, e._2)))) { (r, e) =>
          val (e1, e2) = e
          r flatMap { rr => if (rr) e1.calculateEquals(e2) else Eval.False }
        }
      case _ => Eval.False
    }

    /* Calculate the hash for this modify. */
    override protected def calculateHash(generator: Hash.Generator): Eval[Hash] = for {
      e <- (Eval.now(Vector[Hash]()) /: changes) { (r, e) =>
        for {
          rr <- r
          (k, v) = e
          kk <- k.evalHash(generator)
          vv <- v.evalHash(generator)
        } yield rr :+ kk :+ vv
      }
    } yield generator.hash(Modify.Header, fromHash, e)

    /* Calculate the string for this modify. */
    override protected[data] def calculateToString(): Eval[String] = {
      (Eval.now("") /: changes) { (r, e) =>
        for {
          rr <- r
          (k, v) = e
          kk <- k.calculateToString()
          vv <- v.calculateToString()
        } yield if (rr.isEmpty) s"$kk=$vv" else s"$rr,$kk=$vv"
      } map (c => s"Modify($fromHash,{$c})")
    }

  }

  /**
   * Factory for modifies.
   */
  object Modify extends
    patch_data.Modifies {

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