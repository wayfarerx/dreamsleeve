package net.wayfarerx.dreamsleeve.model

import java.security.MessageDigest

/**
 * Represents an opaque SHA-256 hash.
 *
 * @param bytes The 32 bytes that define the hash.
 */
final class Hash private(private val bytes: Array[Byte]) {

  /* Compare to other hashes. */
  override def equals(that: Any): Boolean = that match {
    case hash: Hash => java.util.Arrays.equals(bytes, hash.bytes)
    case _ => false
  }

  /* Hash the hash. */
  override def hashCode(): Int =
    java.util.Arrays.hashCode(bytes)

  /** Encodes the first 5 bytes in base-16. */
  def toShortString: String =
    toString take 10

  /* Encode in base-16. */
  override def toString: String = {
    val hexChars = new Array[Char](bytes.length * 2)
    for (i <- bytes.indices) {
      val v: Int = bytes(i) & 0xFF
      hexChars(i * 2) = Hash.HexArray(v >>> 4)
      hexChars(i * 2 + 1) = Hash.HexArray(v & 0x0F)
    }
    new String(hexChars)
  }

}

/**
 * Utilities for creating hashes.
 */
object Hash {

  /** The characters to encode a byte array with. */
  private val HexArray = "0123456789abcdef".toCharArray

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
      _hash getOrElse {
        val hash = generateHash(builder)
        _hash = Some(hash)
        hash
      }

    /**
     * Generates and stores a hash for this node.
     *
     * @param builder The hash builder to use.
     * @return A hash for this node.
     */
    protected def generateHash(implicit builder: Hash.Builder): Hash

  }

  /**
   * A builder for hashes.
   */
  final class Builder {

    import Headers._

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
    def hashRevise(fromHash: Hash, title: String, changeHash: Hash): Hash = {
      append(ReviseHeader)
      append(fromHash)
      append(title)
      append(changeHash)
      complete()
    }

    /**
     * Hashes a remove operation.
     *
     * @param hash The hash of the original document being removed.
     */
    def hashRemove(hash: Hash): Hash = {
      append(RemoveHeader)
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
     * Hashes a copy operation.
     *
     * @param fromHash The hash of the value being copied.
     */
    def hashCopy(fromHash: Hash): Hash = {
      append(CopyHeader)
      append(fromHash)
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
     * @param fromHash The hash of the original node.
     * @param hashes   The flat list of edit hashes.
     */
    def hashModify(fromHash: Hash, hashes: Iterable[Hash]): Hash = {
      append(ModifyHeader)
      append(fromHash)
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