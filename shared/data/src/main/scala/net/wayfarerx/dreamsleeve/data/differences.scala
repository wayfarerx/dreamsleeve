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

    /* Generate the hash for this create. */
    override private[data] def generateHash(implicit hasher: Hasher): Hash =
      hasher.hashCreate(document.hash)

  }

  /**
   * Declarations associated with creates.
   */
  object Create extends patching.Creates {

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

    /* Generate the hash for this revise. */
    override private[data] def generateHash(implicit hasher: Hasher): Hash =
      hasher.hashRevise(fromHash, title, update.hash)

  }

  /**
   * Factory for revises.
   */
  object Revise extends patching.Revises {

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
    def apply(from: Document, title: String, update: Update)(implicit hasher: Hasher): Revise =
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
      Revise(from, to.title, Update(from.content, to.content))

  }

  /**
   * Represents when a document is finally deleted.
   *
   * @param fromHash The hash of the document to delete.
   */
  case class Delete(fromHash: Hash) extends Difference {


    /* Generate the hash for this delete. */
    override private[data] def generateHash(implicit hasher: Hasher): Hash =
      hasher.hashDelete(fromHash)

  }

  /**
   * Factory for deletes.
   */
  object Delete extends patching.Deletes {

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

  /**
   * Add a fragment into a table where there was none before.
   *
   * @param toFragment The fragment to add into a table.
   */
  case class Add(toFragment: Fragment) extends Change {

    /* Generate the hash for this add. */
    override private[data] def generateHash(implicit hasher: Hasher): Hash =
      hasher.hashAdd(toFragment.hash)

  }

  /**
   * Declarations associated with adds.
   */
  object Add extends patching.Adds {

    /** The header for adds. */
    val Header: Byte = 0x69.toByte

  }

  /**
   * Remove a fragment from a table.
   *
   * @param fromHash The hash of the fragment to remove from a table.
   */
  case class Remove(fromHash: Hash) extends Change {

    /* Generate the hash for this remove. */
    override private[data] def generateHash(implicit hasher: Hasher): Hash =
      hasher.hashRemove(fromHash)

  }

  /**
   * Factory for removes.
   */
  object Remove extends patching.Removes {

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

}

/**
 * Base type for all changes that update existing fragments.
 */
sealed trait Update extends Change

/**
 * Factory for updates.
 */
object Update extends patching.Updates {

  /**
   * Creates an update by collecting the differences between two fragments.
   *
   * @param from   The fragment that is being updated.
   * @param to     The fragment that is a result of the update.
   * @param hasher The hasher to generate hashes with.
   * @return An update collecting the differences between two fragments.
   */
  def apply(from: Fragment, to: Fragment)(implicit hasher: Hasher): Update = (from, to) match {
    case (f, t) if f == t => Copy(f)
    case ((Value(), _) | (_, Value())) => Replace(from, to)
    case (fromTable@Table(_), toTable@Table(_)) => Modify(fromTable, (
      (toTable.keys -- fromTable.keys map (k => k -> Change.Add(toTable(k)))) ++
        fromTable.keys.map { k =>
          (k, toTable get k map (apply(fromTable(k), _)) getOrElse Change.Remove(fromTable(k)))
        }
      ).toSeq: _*)
  }

  /**
   * Extracts any update implementation.
   *
   * @param update The update to extract.
   * @return True for every update.
   */
  def unapply(update: Update): scala.Boolean =
    true

  /**
   * Copy an existing fragment in a table or document.
   *
   * @param theHash The hash of both fragments.
   */
  case class Copy(theHash: Hash) extends Update {

    /* Generate the theHash for this copy. */
    override private[data] def generateHash(implicit theHasher: Hasher): Hash =
      theHasher.hashCopy(theHash)

  }

  /**
   * Factory for copies.
   */
  object Copy extends patching.Copies {

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

    /* Generate the hash for this replace. */
    override private[data] def generateHash(implicit hasher: Hasher): Hash =
      hasher.hashReplace(fromHash, toFragment.hash)

  }

  /**
   * Factory for replaces.
   */
  object Replace extends patching.Replaces {

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

    /* Generate the hash for this modify. */
    override private[data] def generateHash(implicit hasher: Hasher): Hash =
      hasher.hashModify(fromHash, changes flatMap { case (k, v) => Seq(k.hash, v.hash) })

  }

  /**
   * Factory for modifies.
   */
  object Modify extends patching.Modifies {

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