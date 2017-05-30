package net.wayfarerx.dreamsleeve.model

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
      def updating(key: Value, fromNode: Node, update: Change.Update): Node = {
        path :+= key
        try {
          (fromNode, update) match {
            case (fromTable@Table(_), Change.Modify(fromTableHash, inserts, retains, deletes)) =>
              check(fromTable.hash, fromTableHash)
              var toItems = Vector[(Value, Node)]()
              for ((k, v) <- inserts) {
                if (fromTable.get(k).isDefined) throw new DiffException.StructureMismatch.Unexpected(path, k)
                toItems :+= k -> v.value
              }
              for ((k, v) <- retains) {
                if (fromTable.get(k).isEmpty) throw new DiffException.StructureMismatch.Missing(path, k)
                toItems :+= k -> updating(k, fromTable(k), v)
              }
              for ((k, v) <- deletes) {
                if (fromTable.get(k).isEmpty) throw new DiffException.StructureMismatch.Missing(path, k)
                check(fromTable(k).hash, v)
              }
              Table(toItems: _*)
            case (_, Change.Copy(fromNodeHash)) =>
              check(fromNode.hash, fromNodeHash)
              fromNode
            case (_, Change.Replace(fromNodeHash, toNode)) =>
              check(fromNode.hash, fromNodeHash)
              toNode
            case (_, Change.Modify(fromNodeHash, _, _, _)) =>
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
      def update(fromNode: Node, toNode: Node): Change.Update =
        (fromNode, toNode) match {
          case (f, t) if f.hash == t.hash && f == t =>
            Change.Copy(f.hash)
          case (f@Table(_), t@Table(_)) =>
            val fk = f.keys
            val tk = t.keys
            Change.Modify(f.hash,
              (tk -- fk).toVector map { k => k -> Change.Add(t(k)) },
              (tk & fk).toVector map { k => k -> update(f(k), t(k)) },
              (fk -- tk).toVector map { k => k -> f(k).hash })
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
    case class Replace(fromHash: Hash, toValue: Node) extends Update {

      /* Return the hash for this update. */
      override protected def generateHash(implicit builder: Hash.Builder): Hash =
        builder.hashReplace(fromHash, toValue.hash)

    }

    /**
     * Modifies an existing table inside a table or document.
     *
     * @param fromHash The hash of the original node.
     * @param inserts  The entries to be inserted into the table.
     * @param retains  The entries to be retained in the table.
     * @param deletes  The entries to deleted from the table.
     */
    case class Modify(
      fromHash: Hash,
      inserts: Vector[(Value, Change.Add)],
      retains: Vector[(Value, Change.Update)],
      deletes: Vector[(Value, Hash)]
    ) extends Update {

      /* Return the hash for this modification. */
      override protected def generateHash(implicit builder: Hash.Builder): Hash =
        builder.hashModify(fromHash,
          inserts.flatMap { case (k, v) => Seq(k.hash, v.hash) }
            ++ retains.flatMap { case (k, v) => Seq(k.hash, v.hash) }
            ++ deletes.flatMap { case (k, v) => Seq(k.hash, v) })

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

    }

  }

}
