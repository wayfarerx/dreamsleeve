package net.wayfarerx.dreamsleeve.model

import scala.collection.immutable.{SortedMap, SortedSet}

/**
 * Base class for all diffs applied to a document.
 */
sealed trait Diff extends Hash.Support

/**
 * Implementations of diff operations.
 */
object Diff {

  import Node._

  /**
   * Represents when a document is first created.
   *
   * @param document The document to create.
   */
  case class Create(document: Document) extends Diff {

    /**
     * Returns the resulting document.
     *
     * @return The resulting document.
     */
    def apply(): Document = document

    /* Return the hash for this add operation. */
    override protected def generateHash(implicit builder: Hash.Builder): Hash =
      builder.hashCreate(document.hash)

  }

  /**
   * Represents when an existing document is revised.
   *
   * @param fromHash The hash of the original document.
   * @param title    The title of the resulting document.
   * @param change   The change to apply to the original document's content.
   */
  case class Revise(fromHash: Hash, title: String, change: Change.Update) extends Diff {

    /**
     * Applies all the changes in this revise operation to the specified document.
     *
     * @param from    The document to apply this operation to.
     * @param builder The hash builder to use.
     * @return The document that results from applying this diff to the specified document.
     */
    def apply(from: Document)(implicit builder: Hash.Builder): Document = {
      var path = Vector[Value]()

      /* Verifies that two hashes match. */
      def check(expected: Hash, found: Hash) =
        if (expected != found) throw new DiffException.HashMismatch(path, expected, found)

      /* Applies an update operation to a original node. */
      def updating(key: Value, fromNode: Data, update: Change.Update): Data = {
        path :+= key
        try {
          (fromNode, update) match {
            case (fromTable@Table(_), Change.Modify(fromTableHash, changes)) =>
              check(fromTable.hash, fromTableHash)
              if ((fromTable.keys -- changes.keySet).nonEmpty)
                throw new DiffException.StructureMismatch.Undefined(path, fromTable.keys -- changes.keySet)
              var toItems = Vector[(Value, Data)]()
              for ((k, v) <- changes) v match {
                case Change.Add(data) =>
                  if (fromTable.get(k).isDefined) throw new DiffException.StructureMismatch.Unexpected(path, k)
                  toItems :+= k -> data
                case update: Change.Update =>
                  if (fromTable.get(k).isEmpty) throw new DiffException.StructureMismatch.Missing(path, k)
                  toItems :+= k -> updating(k, fromTable(k), update)
                case Change.Delete(hash) =>
                  if (fromTable.get(k).isEmpty) throw new DiffException.StructureMismatch.Missing(path, k)
                  check(fromTable(k).hash, hash)
              }
              Table(toItems: _*)
            case (_, Change.Copy(fromNodeHash)) =>
              check(fromNode.hash, fromNodeHash)
              fromNode
            case (_, Change.Replace(fromNodeHash, toNode)) =>
              check(fromNode.hash, fromNodeHash)
              toNode
            case (_, Change.Modify(fromNodeHash, _)) =>
              check(fromNode.hash, fromNodeHash)
              throw new DiffException.StructureMismatch.Type(path)
          }
        } finally path = path.init
      }

      check(from.hash, fromHash)
      Document(title, updating(Value.String(from.title), from.content, change))
    }

    /* Return the hash for this revision. */
    override protected def generateHash(implicit builder: Hash.Builder): Hash =
      builder.hashRevise(fromHash, title, change.hash)

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
      def update(fromNode: Data, toNode: Data): Change.Update =
        (fromNode, toNode) match {
          case (f, t) if f.hash == t.hash && f == t =>
            Change.Copy(f.hash)
          case (f@Table(_), t@Table(_)) =>
            Change.Modify(f.hash, SortedMap((f.keys ++ t.keys).toSeq map { k =>
              f.get(k) -> t.get(k) match {
                case (None, Some(tv)) => k -> Change.Add(tv)
                case (Some(fv), Some(tv)) => k -> update(fv, tv)
                case (Some(fv), None) => k -> Change.Delete(fv.hash)
                case (None, None) => sys.error("unreachable")
              }
            }: _*))
          case (f, t) =>
            Change.Replace(f.hash, t)
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

    /**
     * Applies all this remove operation to the specified document.
     *
     * @param from    The document to apply this operation to.
     * @param builder The hash builder to use.
     */
    def apply(from: Document)(implicit builder: Hash.Builder): Unit =
      if (fromHash != from.hash) throw new DiffException.HashMismatch(Vector.empty, fromHash, from.hash)


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
    case class Add(value: Data) extends Change {

      /* Return the hash for this addition. */
      override protected def generateHash(implicit builder: Hash.Builder): Hash =
        builder.hashAdd(value.hash)

    }

    /**
     * Base class for all changes that update existing data.
     */
    sealed trait Update extends Change

    /**
     * Copies an existing node in a table or document.
     *
     * @param fromHash The hash of the original node.
     */
    case class Copy(fromHash: Hash) extends Update {

      /* Return the hash for this update. */
      override protected def generateHash(implicit builder: Hash.Builder): Hash =
        builder.hashCopy(fromHash)

    }

    /**
     * Replaces an existing node in a table or document.
     *
     * @param fromHash The hash of the original node.
     * @param toValue  The value to replace the original node with.
     */
    case class Replace(fromHash: Hash, toValue: Data) extends Update {

      /* Return the hash for this update. */
      override protected def generateHash(implicit builder: Hash.Builder): Hash =
        builder.hashReplace(fromHash, toValue.hash)

    }

    /**
     * Modifies an existing table inside a table or document.
     *
     * @param fromHash The hash of the original node.
     * @param changes  The changes to be applied to the table.
     */
    case class Modify(fromHash: Hash, changes: SortedMap[Value, Change]) extends Update {

      /* Return the hash for this modification. */
      override protected def generateHash(implicit builder: Hash.Builder): Hash =
        builder.hashModify(fromHash, changes flatMap { case (k, v) => Seq(k.hash, v.hash) })

    }

    /**
     * Deletes a node from a table.
     *
     * @param hash The hash of the value to remove from a table or document.
     */
    case class Delete(hash: Hash) extends Change {

      /* Return the hash for this deletion. */
      override protected def generateHash(implicit builder: Hash.Builder): Hash =
        builder.hashDelete(hash)

    }

  }

  abstract class DiffException(msg: String) extends RuntimeException(msg)

  object DiffException {

    final class HashMismatch(path: Vector[Value], expected: Hash, found: Hash) extends DiffException(
      s"Hash mismatch at ${path mkString "/"} expected ${expected.toShortString} found ${found.toShortString}")

    abstract class StructureMismatch(msg: String) extends DiffException(msg)

    object StructureMismatch {

      final class Type(path: Vector[Value])
        extends StructureMismatch(s"Type mismatch at ${path mkString "/"} expected value found a table")

      final class Missing(path: Vector[Value], expected: Value)
        extends StructureMismatch(s"Missing at ${path mkString "/"} expected $expected found nothing")

      final class Unexpected(path: Vector[Value], found: Value)
        extends StructureMismatch(s"Unexpected at ${path mkString "/"} expected nothing found $found")

      final class Undefined(path: Vector[Value], missing: SortedSet[Value])
        extends StructureMismatch(s"Type mismatch at ${path mkString "/"} keys not defined: ${missing mkString ","}.")

    }

  }

}
