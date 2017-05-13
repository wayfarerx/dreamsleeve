package net.wayfarerx.dreamsleeve.model

import net.wayfarerx.dreamsleeve.diff.Differences

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
    override protected def generateHash(implicit builder: Hash.Builder): Hash =
      builder.hashCreate(document.hash)

  }

  /**
   * Represents when an existing document is revised.
   *
   * @param fromHash The hash of the original document.
   * @param title    The title of the resulting document.
   * @param change   The optional change to apply to the original document's content.
   */
  case class Revise(fromHash: Hash, title: String, change: Option[Change.Update]) extends Diff {

    /* Return the hash for this revision. */
    override protected def generateHash(implicit builder: Hash.Builder): Hash =
      builder.hashRevise(fromHash, title, change.map(_.hash))

  }

  /**
   * Factory for revise operations.
   */
  object Revise {

    /**
     * Creates a revise operation for the specified documents.
     *
     * @param from    The original document to create the revise operation for.
     * @param to      The resulting document to create the revise operation for.
     * @param builder The hash builder to use.
     * @return A revise operation for the specified documents.
     */
    def apply(from: Document, to: Document)(implicit builder: Hash.Builder): Revise = {

      def change(fromNodeOpt: Option[Node], toNode: Node): Option[Change] =
        fromNodeOpt match {
          case Some(fromNode) => update(fromNode, toNode)
          case None => Some(Change.Add(toNode))
        }

      /* Creates an update operation for the specified nodes if they differ. */
      def update(fromNode: Node, toNode: Node): Option[Change.Update] =
        (fromNode, toNode) match {
          case (f, t) if f.hash == t.hash && f == t =>
            None
          case (f@Table(_), t@Table(_)) =>
            Some(Change.Modify(Differences(f.keys, t.keys) map {
              case Differences.Insert(v) => Edit.Insert(v map { k => k -> change(f.get(k), t(k)) })
              case Differences.Retain(v) => Edit.Retain(v map { k => k.hash -> change(f.get(k), t(k)) })
              case Differences.Delete(v) => Edit.Delete(v map (_.hash))
            }))
          case (f, t) =>
            Some(Change.Replace(f.hash, t))
        }

      Revise(from.hash, to.title, update(from.content, to.content))
    }

  }

  /**
   * Represents when a document is finally removed.
   *
   * @param fromHash The hash of the document to remove.
   */
  case class Remove(fromHash: Hash) extends Diff {

    /* Return the hash for this remove operation. */
    override protected def generateHash(implicit builder: Hash.Builder): Hash =
      builder.hashRemove(fromHash)

  }

  /**
   * Factory for remove operations.
   */
  object Remove {

    /**
     * Creates a remove operation for the specified document.
     *
     * @param document The document to create the remove operation for.
     * @param builder  The hash builder to use.
     * @return A remove operation for the specified document.
     */
    def apply(document: Document)(implicit builder: Hash.Builder): Remove =
      Remove(document.hash)

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
    override protected def generateHash(implicit builder: Hash.Builder): Hash =
      builder.hashAdd(value.hash)

  }

  /**
   * Base class for all changes that update existing data.
   */
  sealed trait Update extends Change

  /**
   * Replaces an existing node in a table or document.
   *
   * @param fromHash The hash of the original node.
   * @param toValue  The value to replace the original node with.
   */
  case class Replace(fromHash: Hash, toValue: Node) extends Update {

    /* Return the hash for this update. */
    override protected def generateHash(implicit builder: Hash.Builder): Hash =
      builder.hashReplace(fromHash, toValue.hash)

  }

  /**
   * Modifies an existing table inside a table or document.
   *
   * @param edits The edits to the list of keys in the original table.
   */
  case class Modify(edits: Vector[Edit]) extends Update {

    /* Return the hash for this modification. */
    override protected def generateHash(implicit builder: Hash.Builder): Hash =
      builder.hashModify(edits map (_.hash))

  }

}

/**
 * Base class for edits that are applied to the list of items in a table.
 */
sealed trait Edit extends Hash.Support

/**
 * Implementations of edit operations.
 */
object Edit {

  /**
   * Represents the insertion of items into a table.
   *
   * @param items The keys with their associated value changes to be inserted.
   */
  case class Insert(items: Vector[(Value, Option[Change])]) extends Edit {

    /* Return the hash for this insert operation. */
    override protected def generateHash(implicit builder: Hash.Builder): Hash =
      builder.hashInsert(items map { case (k, v) => k.hash -> v.map(_.hash) })

  }

  /**
   * Represents the retaining of items between tables.
   *
   * @param items The hashes of the keys with their associated value changes to be retained.
   */
  case class Retain(items: Vector[(Hash, Option[Change])]) extends Edit {

    /* Return the hash for this retain operation. */
    override protected def generateHash(implicit builder: Hash.Builder): Hash =
      builder.hashRetain(items map { case (k, v) => k -> v.map(_.hash) })

  }

  /**
   * Represents the deletion of items from a table.
   *
   * @param keyHashes The hashes of the item keys to be deleted.
   */
  case class Delete(keyHashes: Vector[Hash]) extends Edit {

    /* Return the hash for this delete operation. */
    override protected def generateHash(implicit builder: Hash.Builder): Hash =
      builder.hashDelete(keyHashes)

  }

}
