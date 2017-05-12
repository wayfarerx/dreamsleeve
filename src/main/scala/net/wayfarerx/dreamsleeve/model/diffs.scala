package net.wayfarerx.dreamsleeve.model

import collection.immutable.{ListMap, ListSet}

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
   * @param fromHash The hash of the original document.
   * @param title    The title of the resulting document.
   * @param change   The optional change to apply to the original document's content.
   */
  case class Revise(fromHash: Hash, title: String, change: Option[Change.Update]) extends Diff {

    /* Return the hash for this revision. */
    override protected def hashWith(builder: Hash.Builder): Hash =
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

      /* Creates an update operation for the specified nodes if they differ. */
      def update(fromNode: Node, toNode: Node): Option[Change.Update] =
        (fromNode, toNode) match {
          case (from, to) if from.hash == to.hash && from == to =>
            None
          case (from@Table(_), to@Table(_)) =>
            Some(Change.Modify(edits(from.keys.toVector, to.keys.toVector), ListMap(to.keys.toSeq.flatMap {
              case k if from.keys(k) => update(from(k), to(k)).map(k -> _)
              case k => Some(k -> Change.Add(to(k)))
            }: _*)))
          case (from, to) =>
            Some(Change.Replace(from.hash, to))
        }

      /* Creates edit operations that transform one set of keys to another. */
      def edits(fromKeys: Vector[Value], toKeys: Vector[Value]): Vector[Edit] = {
        ??? // FIXME
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
    override protected def hashWith(builder: Hash.Builder): Hash =
      builder.hashDelete(fromHash)

  }

  /**
   * Factory for remove operations.
   */
  object Remove {

    /**
     * Creates a remove operation for the specified document.
     *
     * @param document The document to create the remove operation for.
     * @return A remove operation for the specified document.
     */
    def apply(document: Document): Remove =
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
   * @param toValue  The value to replace the original node with.
   */
  case class Replace(fromHash: Hash, toValue: Node) extends Update {

    /* Return the hash for this update. */
    override protected def hashWith(builder: Hash.Builder): Hash =
      builder.hashReplace(fromHash, toValue.hash(builder))

  }

  /**
   * Modifies an existing table inside a table or document.
   *
   * @param edits   The edits to the list of keys in the original table.
   * @param changes The changes to the values in the original table.
   */
  case class Modify(edits: Vector[Edit], changes: ListMap[Value, Change]) extends Update {

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
   * @param keys The keys to be inserted.
   */
  case class Insert(keys: Vector[Value]) extends Edit {

    /* Return the hash for this insert operation. */
    override protected def hashWith(builder: Hash.Builder) =
      builder.hashInsert(keys map (_.hash(builder)))

  }

  /**
   * Represents the retaining of keys between tables.
   *
   * @param hashes The hashes of the keys to be retained.
   */
  case class Retain(hashes: Vector[Hash]) extends Edit {

    /* Return the hash for this copy operation. */
    override protected def hashWith(builder: Hash.Builder) =
      builder.hashCopy(hashes)

  }

  /**
   * Represents the deletion of keys from a table.
   *
   * @param hashes The hashes of the keys to be deleted.
   */
  case class Delete(hashes: Vector[Hash]) extends Edit {

    /* Return the hash for this remove operation. */
    override protected def hashWith(builder: Hash.Builder) =
      builder.hashRemove(hashes)

  }

}
