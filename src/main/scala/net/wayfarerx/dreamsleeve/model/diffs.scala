package net.wayfarerx.dreamsleeve.model

import collection.immutable.Map

/**
 * Base class for all diffs applied to a document.
 */
sealed trait Diff extends Hash.Support

/**
 * Implementations of diff operations.
 */
object Diff {

  /**
   * Represents when a document is first created.
   *
   * @param document The document to create.
   */
  case class Create(document: Document) extends Diff {

    /* Return the hash for this add operation. */
    override protected def hashWith(builder: Hash.Builder): Hash =
      builder.hashCreate(document.hash(builder))

  }

  /**
   * Represents when an existing document is revised.
   *
   * @param title The title of the resulting document.
   * @param change The change to apply to the original document's content.
   * @param fromHash The hash of the original document.
   */
  case class Revise(title: String, change: Change.Update, fromHash: Hash) extends Diff {

    /* Return the hash for this revision. */
    override protected def hashWith(builder: Hash.Builder): Hash =
      builder.hashRevise(title, change.hash(), fromHash)

  }

  /**
   * Represents when a document is finally deleted.
   *
   * @param fromHash The hash of the document to delete.
   */
  case class Delete(fromHash: Hash) extends Diff {

    /* Return the hash for this add operation. */
    override protected def hashWith(builder: Hash.Builder): Hash =
      builder.hashDelete(fromHash)

  }

}

/**
 * Base class for all changes applied to values in a table or document.
 */
sealed trait Change extends Hash.Support

/**
 * Implementations of change operations.
 */
object Change {

  /**
   * Adds a node into a table where there was none before.
   *
   * @param value The value to add into a table or document.
   */
  case class Add(value: Node) extends Change {

    /* Return the hash for this addition. */
    override protected def hashWith(builder: Hash.Builder): Hash =
      builder.hashAdd(value.hash(builder))

  }

  /**
   * Base class for all changes that update existing data.
   */
  sealed trait Update extends Change

  /**
   * Replaces an existing node in a table or document.
   *
   * @param fromHash The hash of the original node.
   * @param toValue The value to replace the original node with.
   */
  case class Replace(fromHash: Hash, toValue: Node) extends Update {

    /* Return the hash for this update. */
    override protected def hashWith(builder: Hash.Builder): Hash =
      builder.hashReplace(fromHash, toValue.hash(builder))

  }

  /**
   * Modifies an existing table inside a table or document.
   *
   * @param edits The edits to the list of keys in the original table.
   * @param changes The changes to the values in the original table.
   */
  case class Modify(edits: Vector[Edit], changes: Map[Value, Change]) extends Update {

    /* Return the hash for this modification. */
    override protected def hashWith(builder: Hash.Builder): Hash =
      builder.hashModify(edits map (_.hash(builder)),
        changes flatMap { case (k, v) => Vector(k.hash(builder), v.hash(builder)) })

  }

}

/**
 * Base class for edits that are applied to the list of keys in a table.
 */
sealed trait Edit extends Hash.Support

/**
 * Implementations of edit operations.
 */
object Edit {

  /**
   * Represents the insertion of keys into a table.
   *
   * @param index The index in the original table to insert at.
   * @param keys The keys to be inserted.
   */
  case class Insert(index: Int, keys: Vector[Value]) extends Edit {

    /* Return the hash for this insert operation. */
    override protected def hashWith(builder: Hash.Builder) =
      builder.hashInsert(index, keys map (_.hash(builder)))

  }

  /**
   * Represents the removal of keys from a table.
   *
   * @param index The index in the original table to remove at.
   * @param hashes The hashes of the keys to be removed.
   */
  case class Remove(index: Int, hashes: Vector[Hash]) extends Edit {

    /* Return the hash for this remove operation. */
    override protected def hashWith(builder: Hash.Builder) =
      builder.hashRemove(index, hashes)

  }

}
