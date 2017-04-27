package net.wayfarerx.dreamsleeve.model

import java.util.Arrays
import java.security.MessageDigest

/**
 * Represents an opaque SHA-256 hash.
 *
 * @param bytes The 32 bytes that define the hash.
 */
final class Hash private (private val bytes: Array[Byte]) {

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
    bytes map { _.toHexString } mkString ""

}

/**
 * Utilities for creating hashes.
 */
object Hash {

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
      digest.reset()
      digest.update(BooleanHeader)
      digest.update(if (value) 0xFF.toByte else 0x00.toByte)
      new Hash(digest.digest())
    }

    /**
     * Hashes a double value.
     *
     * @param value The value to hash.
     */
    def hashDouble(value: Double): Hash = {
      val bits = java.lang.Double.doubleToRawLongBits(value)
      digest.reset()
      digest.update(DoubleHeader)
      digest.update((bits >>> 56 & 0x00000000000000FF).toByte)
      digest.update((bits >>> 48 & 0x00000000000000FF).toByte)
      digest.update((bits >>> 40 & 0x00000000000000FF).toByte)
      digest.update((bits >>> 32 & 0x00000000000000FF).toByte)
      digest.update((bits >>> 24 & 0x00000000000000FF).toByte)
      digest.update((bits >>> 16 & 0x00000000000000FF).toByte)
      digest.update((bits >>> 8 & 0x00000000000000FF).toByte)
      digest.update((bits & 0x00000000000000FF).toByte)
      new Hash(digest.digest())
    }

    /**
     * Hashes a string value.
     *
     * @param value The value to append.
     */
    def hashString(value: String): Hash = {
      digest.reset()
      digest.update(StringHeader)
      value foreach { c =>
        digest.update((c >>> 8 & 0x00FF).toByte)
        digest.update((c & 0x00FF).toByte)
      }
      new Hash(digest.digest())
    }

    /**
     * Hashes a table.
     *
     * @param value The value to append.
     */
    def hashTable(hashes: Iterable[Hash]): Hash = {
      digest.reset()
      digest.update(TableHeader)
      hashes foreach { hash => digest.update(hash.bytes) }
      new Hash(digest.digest())
    }

  }

  /**
   * Factory for creating builders.
   */
  object Builder {

    /** The header for boolean hashes. */
    private val BooleanHeader = 0x1E.toByte
    /** The header for double hashes. */
    private val DoubleHeader = 0x5A.toByte
    /** The header for string hashes. */
    private val StringHeader = 0xA5.toByte
    /** The header for table hashes. */
    private val TableHeader = 0xE1.toByte

    /**
     * Creates a new hash builder.
     *
     * @return A new hash builder.
     */
    def apply(): Builder = new Builder

  }

}