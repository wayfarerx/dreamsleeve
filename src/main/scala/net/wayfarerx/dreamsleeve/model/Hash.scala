package net.wayfarerx.dreamsleeve.model

import java.util.Arrays
import java.security.MessageDigest

/**
 * Represents an opaque 160-bit SHA-1 hash.
 *
 * @param bytes The 20 bytes that define the hash.
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
   * A mutable builder for hashes.
   *
   * @param digest The message digest to use.
   */
  final class Builder private (private val digest: MessageDigest) {
    import Builder._

    /**
     * Appends a boolean value.
     *
     * @param value The value to append.
     */
    def appendBoolean(value: Boolean): Unit = {
      digest.update(BooleanHeader)
      digest.update(if (value) 0xFF.toByte else 0x00.toByte)
    }

    /**
     * Appends a double value.
     *
     * @param value The value to append.
     */
    def appendDouble(value: Double): Unit = {
      digest.update(DoubleHeader)
      val bits = java.lang.Double.doubleToRawLongBits(value)
      digest.update((bits >>> 56 & 0x00000000000000FF).toByte)
      digest.update((bits >>> 48 & 0x00000000000000FF).toByte)
      digest.update((bits >>> 40 & 0x00000000000000FF).toByte)
      digest.update((bits >>> 32 & 0x00000000000000FF).toByte)
      digest.update((bits >>> 24 & 0x00000000000000FF).toByte)
      digest.update((bits >>> 16 & 0x00000000000000FF).toByte)
      digest.update((bits >>> 8 & 0x00000000000000FF).toByte)
      digest.update((bits & 0x00000000000000FF).toByte)
    }

    /**
     * Appends a string value.
     *
     * @param value The value to append.
     */
    def appendString(value: String): Unit = {
      digest.update(StringHeader)
      digest.update(value.getBytes("UTF-8"))
      digest.update(StringFooter)
    }

    /**
     * Appends a table.
     *
     * @param value The value to append.
     */
    def appendTable(next: => Unit): Unit = {
      digest.update(TableHeader)
      next
      digest.update(TableFooter)
    }

    /**
     * Generates the ultimate hash.
     *
     * @return The hash of all appended data.
     */
    def complete(): Hash =
      new Hash(digest.digest())

  }

  /**
   * Globals variables and functions related to hashers.
   */
  object Builder {

    /** The header for boolean hashes. */
    private val BooleanHeader = 0x1F.toByte
    /** The header for double hashes. */
    private val DoubleHeader = 0x2E.toByte
    /** The header for string hashes. */
    private val StringHeader = 0x3D.toByte
    /** The footer for string hashes. */
    private val StringFooter = 0x4C.toByte
    /** The header for table hashes. */
    private val TableHeader = 0x5B.toByte
    /** The footer for table hashes. */
    private val TableFooter = 0x6A.toByte

    /**
     * Creates a new hash builder.
     *
     * @return A new hash builder.
     */
    def apply(): Builder =
      new Builder(MessageDigest.getInstance("SHA-1"))

  }

}