package net.wayfarerx.dreamsleeve.model

import java.util.Arrays
import java.security.MessageDigest

/**
 * Represents an opaque SHA-256 hash.
 *
 * @param bytes The 32 bytes that define the hash.
 */
final class Hash private(private val bytes: Array[Byte]) {

  /* Compare to other hashes. */
  override def equals(that: Any) = that match {
    case hash: Hash => Arrays.equals(bytes, hash.bytes)
    case _ => false
  }

  /* Hash the hash. */
  override def hashCode() =
    Arrays.hashCode(bytes)

  /* Encode in base-16. */
  override def toString(): String =
    bytes map {
      _.toHexString
    } mkString ""

}

/**
 * Utilities for creating hashes.
 */
object Hash {

  /**
   * Mixin that supports common hashing operations.
   */
  trait Support {

    /** The cached hash of this node. */
    @volatile private var _hash: Option[Hash] = None

    /**
     * Returns a hash for this node.
     *
     * @return A hash for this node.
     */
    final def hash(implicit builder: Builder): Hash =
      _hash getOrElse generateHash(builder)

    /**
     * Generates and stores a hash for this node.
     *
     * @param builder The hash builder to use.
     * @return A hash for this node.
     */
    private def generateHash(builder: Hash.Builder): Hash = {
      val hash = hashWith(builder)
      _hash = Some(hash)
      hash
    }

    /**
     * Generates a hash for this node.
     *
     * @param builder The hash builder to use.
     * @return A hash for this node.
     */
    protected def hashWith(builder: Hash.Builder): Hash

  }

  /**
   * A builder for hashes.
   */
  final class Builder {

    import Builder._

    /** The message digest to use. */
    private val digest = MessageDigest.getInstance("SHA-256")

    /**
     * Hashes a boolean value.
     *
     * @param value The value to hash.
     */
    def hashBoolean(value: Boolean): Hash = {
      append(BooleanHeader)
      append(value)
      complete()
    }

    /**
     * Hashes a number value.
     *
     * @param value The value to hash.
     */
    def hashNumber(value: Double): Hash = {
      append(NumberHeader)
      append(java.lang.Double.doubleToRawLongBits(value))
      complete()
    }

    /**
     * Hashes a string value.
     *
     * @param value The value to hash.
     */
    def hashString(value: String): Hash = {
      append(StringHeader)
      append(value)
      complete()
    }

    /**
     * Hashes a table.
     *
     * @param hashes The entry hashes to hash.
     */
    def hashTable(hashes: Iterable[Hash]): Hash = {
      append(TableHeader)
      hashes foreach append
      complete()
    }

    /**
     * Hashes a document.
     *
     * @param title       The title of the document to hash.
     * @param contentHash The hash of the document's content to hash.
     */
    def hashDocument(title: String, contentHash: Hash): Hash = {
      append(DocumentHeader)
      append(title)
      append(contentHash)
      complete()
    }

    /**
     * Hashes a create operation.
     *
     * @param hash The hash of the resulting document being created.
     */
    def hashCreate(hash: Hash): Hash = {
      append(CreateHeader)
      append(hash)
      complete()
    }

    /**
     * Hashes a collection of changes.
     *
     * @param fromHash   The hash of the original document's content.
     * @param title      The title of the resulting document.
     * @param changeHash The hash of the change to apply to the original document.
     */
    def hashRevise(fromHash: Hash, title: String, changeHash: Option[Hash]): Hash = {
      append(ReviseHeader)
      append(fromHash)
      append(title)
      changeHash foreach append
      complete()
    }

    /**
     * Hashes a delete operation.
     *
     * @param hash The hash of the original document being deleted.
     */
    def hashDelete(hash: Hash): Hash = {
      append(DeleteHeader)
      append(hash)
      complete()
    }

    /**
     * Hashes an add operation.
     *
     * @param hash The hash of the value being added.
     */
    def hashAdd(hash: Hash): Hash = {
      append(AddHeader)
      append(hash)
      complete()
    }

    /**
     * Hashes a replace operation.
     *
     * @param fromHash The hash of the value being replaced.
     * @param toHash   The hash of the value doing the replacing.
     */
    def hashReplace(fromHash: Hash, toHash: Hash): Hash = {
      append(ReplaceHeader)
      append(fromHash)
      append(toHash)
      complete()
    }

    /**
     * Hashes a modify operation.
     *
     * @param editHashes   The flattened list of edit hashes.
     * @param changeHashes The flattened list of change hashes.
     */
    def hashModify(editHashes: Iterable[Hash], changeHashes: Iterable[Hash]): Hash = {
      append(ModifyHeader)
      editHashes foreach append
      changeHashes foreach append
      complete()
    }

    /**
     * Hashes a table key list copy operation.
     *
     * @param hashes The hashes of the nodes being copied.
     */
    def hashCopy(hashes: Iterable[Hash]): Hash = {
      append(CopyHeader)
      hashes foreach append
      complete()
    }

    /**
     * Hashes a table key list insert operation.
     *
     * @param hashes The hashes of the nodes being inserted.
     */
    def hashInsert(hashes: Iterable[Hash]): Hash = {
      append(InsertHeader)
      hashes foreach append
      complete()
    }

    /**
     * Hashes a table key list remove operation.
     *
     * @param hashes The hashes of the nodes being removed.
     */
    def hashRemove(hashes: Iterable[Hash]): Hash = {
      append(RemoveHeader)
      hashes foreach append
      complete()
    }

    /**
     * Appends a boolean value to the running digest.
     *
     * @param value The value to append.
     */
    private def append(value: Boolean): Unit =
      digest.update(if (value) 0xFF.toByte else 0x00.toByte)

    /**
     * Appends a byte value to the running digest.
     *
     * @param value The value to append.
     */
    private def append(value: Byte): Unit =
      digest.update(value)

    /**
     * Appends a character value to the running digest.
     *
     * @param value The value to append.
     */
    private def append(value: Char): Unit = {
      digest.update((value >>> 8 & 0x00FF).toByte)
      digest.update((value & 0x00FF).toByte)
    }

    /**
     * Appends a long value to the running digest.
     *
     * @param value The value to append.
     */
    private def append(value: Long): Unit = {
      digest.update((value >>> 56 & 0x00000000000000FF).toByte)
      digest.update((value >>> 48 & 0x00000000000000FF).toByte)
      digest.update((value >>> 40 & 0x00000000000000FF).toByte)
      digest.update((value >>> 32 & 0x00000000000000FF).toByte)
      digest.update((value >>> 24 & 0x00000000000000FF).toByte)
      digest.update((value >>> 16 & 0x00000000000000FF).toByte)
      digest.update((value >>> 8 & 0x00000000000000FF).toByte)
      digest.update((value & 0x00000000000000FF).toByte)
    }

    /**
     * Appends a string value to the running digest.
     *
     * @param value The value to append.
     */
    private def append(value: String): Unit =
      value foreach append

    /**
     * Appends a hash to the running digest.
     *
     * @param hash The hash to append.
     */
    private def append(hash: Hash): Unit =
      digest.update(hash.bytes)

    /**
     * Completes the running digest and generates a hash.
     *
     * @return The resulting hash from the running digest.
     */
    private def complete(): Hash =
      new Hash(digest.digest())


  }

  /**
   * Factory for creating builders.
   */
  object Builder {

    /** The header for document hashes. */
    private val DocumentHeader = 0xE1.toByte
    /** The header for table hashes. */
    private val TableHeader = 0xD2.toByte
    /** The header for string hashes. */
    private val StringHeader = 0xC3.toByte
    /** The header for number hashes. */
    private val NumberHeader = 0xB4.toByte
    /** The header for boolean hashes. */
    private val BooleanHeader = 0xA5.toByte
    /** The header for document creation. */
    private val CreateHeader = 0x96.toByte
    /** The header for change document revisions. */
    private val ReviseHeader = 0x87.toByte
    /** The header for document deletion. */
    private val DeleteHeader = 0x78.toByte
    /** The header for add operation hashes. */
    private val AddHeader = 0x69.toByte
    /** The header for replace operation hashes. */
    private val ReplaceHeader = 0x5A.toByte
    /** The header for modify operation hashes. */
    private val ModifyHeader = 0x4B.toByte
    /** The header for copy operation hashes. */
    private val CopyHeader = 0x3C.toByte
    /** The header for insert operation hashes. */
    private val InsertHeader = 0x2D.toByte
    /** The header for remove operation hashes. */
    private val RemoveHeader = 0x1E.toByte

    /**
     * Implicitly creates a new hash builder.
     *
     * @return A new hash builder.
     */
    implicit def newImplicitBuilder: Builder = apply()

    /**
     * Creates a new hash builder.
     *
     * @return A new hash builder.
     */
    def apply(): Builder = new Builder

  }

}