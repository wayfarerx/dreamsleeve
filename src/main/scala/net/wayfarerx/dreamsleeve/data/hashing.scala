/*
 * hashing.scala
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

import java.security.MessageDigest

/**
 * Represents an immutable SHA-256 hash.
 *
 * @param bytes The 32 bytes that define the hash.
 */
final class Hash private(private val bytes: Array[Byte]) {

  import Hash._

  /* Compare to other hashes. */
  override def equals(that: Any): Boolean = that match {
    case hash: Hash => bytes sameElements hash.bytes
    case _ => false
  }

  /* Hash the hash. */
  override def hashCode(): Int =
    java.util.Arrays.hashCode(bytes)

  /**
   * Returns a copy of this hash's bytes.
   *
   * @return A copy of this hash's bytes.
   */
  def toBytes: Array[Byte] =
    bytes.clone()

  /**
   * Encodes the specified number of bytes in base-16.
   *
   * @param count The number of bytes to encode, defaults to five.
   * @return An encoded representation of the first `count` bytes.
   */
  def toPrefixString(count: Int = 5): String =
    if (count >= 0 && count <= Size) {
      val characters = new Array[Char](count * 2)
      for (i <- 0 until count) {
        val v: Int = bytes(i) & 0xFF
        characters(i * 2) = Base16(v >>> 4)
        characters(i * 2 + 1) = Base16(v & 0x0F)
      }
      new String(characters)
    } else throw new IllegalArgumentException(
      s"Incorrect number of bytes for a hash prefix, expected [0, $Size] found $count.")

  /* Encodes all the bytes in base-16. */
  override def toString: String =
    toPrefixString(Size)

}

/**
 * Utilities for creating and working with hashes.
 */
object Hash {

  /** The number of bytes in a hash. */
  val Size = 32

  /** The characters to encode a byte array with. */
  private val Base16 = "0123456789abcdef"

  /**
   * Creates a new hash from the specified binary representation.
   *
   * @param bytes The 32 bytes that represent the hash to create.
   * @return A new hash from the specified binary representation.
   */
  def apply(bytes: Array[Byte]): Hash =
    if (bytes.length == Size) new Hash(bytes.clone())
    else throw new IllegalArgumentException(
      s"Incorrect number of bytes for a hash, expected $Size found ${bytes.length}.")

  /**
   * Creates a new hash from the specified base-16 textual representation.
   *
   * @param characters The 64 base-16 characters that represent the hash to create.
   * @return A new hash from the specified base-16 textual representation.
   */
  def apply(characters: Array[Char]): Hash =
    if (characters.length == Size * 2) {
      val bytes = new Array[Byte](Size)
      for (i <- 0 until Size) {
        val high = Base16.indexOf(characters(i * 2).toLower)
        if (high < 0) throw new IllegalArgumentException(
          s"Invalid character in encoded hash representation: ${characters(i * 2)}.")
        val low = Base16.indexOf(characters(i * 2 + 1).toLower)
        if (low < 0) throw new IllegalArgumentException(
          s"Invalid character in encoded hash representation: ${characters(i * 2 + 1)}.")
        bytes(i) = ((high << 4 | low) & 0xFF).toByte
      }
      new Hash(bytes)
    } else throw new IllegalArgumentException(
      s"Incorrect number of base-16 characters for a hash, expected ${Size * 2} found ${characters.length}.")

  /**
   * Creates a new hash from the specified base-16 textual representation.
   *
   * @param characters The 64-character, base-16 string that represents the hash to create.
   * @return A new hash from the specified base-16 textual representation.
   */
  def apply(characters: String): Hash =
    apply(characters.toCharArray)

  /**
   * A utility that allows a hasher to access the internal byte array of a hash.
   */
  private[data] trait Access {

    /**
     * Creates a new hash without cloning the byte array.
     *
     * @param bytes The bytes to use as the internal byte array for the new hash.
     * @return A new hash without cloning the byte array.
     */
    protected final def createHash(bytes: Array[Byte]): Hash =
      new Hash(bytes)

    /**
     * Returns the internal byte array of the specified hash.
     *
     * @return The internal byte array of the specified hash.
     */
    protected final def internalBytes(hash: Hash): Array[Byte] =
      hash.bytes

  }

}

/**
 * Base type for data elements that can be hashed.
 */
trait Hashable {

  /** The cached hash of this data element. */
  private var _hash: Option[Hash] = None

  /**
   * Returns the hash for this data element, generating it if necessary.
   *
   * @param hasher The hasher to generate a hash with if necessary.
   * @return The hash for this data element.
   */
  final def hash(implicit hasher: Hasher): Hash =
    _hash getOrElse {
      val hash = generateHash(hasher)
      _hash = Some(hash)
      hash
    }

  /**
   * Generates the hash for this data element.
   *
   * @param hasher The hasher to generate a hash with.
   * @return The hash for this data element.
   */
  private[data] def generateHash(implicit hasher: Hasher): Hash

}

/**
 * Utility that generates hashes for data elements.
 */
final class Hasher private extends Hash.Access {

  /** The message digest to use. */
  private val digest = MessageDigest.getInstance("SHA-256")

  /**
   * Hashes a document.
   *
   * @param title       The title of the document to compute the hash from.
   * @param contentHash The hash of the document's content to compute the hash from.
   * @return The hash generated for the specified document.
   */
  private[data] def hashDocument(title: String, contentHash: Hash): Hash = {
    append(Document.Header)
    append(title)
    append(contentHash)
    complete()
  }

  /**
   * Hashes a table.
   *
   * @param entryHashes The entry hashes to compute the hash from.
   * @return The hash generated for the specified table.
   */
  private[data] def hashTable(entryHashes: Iterable[Hash]): Hash = {
    append(Table.Header)
    entryHashes foreach append
    complete()
  }

  /**
   * Hashes a boolean.
   *
   * @param value The boolean to compute the hash from.
   * @return The hash generated for the specified boolean.
   */
  private[data] def hashBoolean(value: Boolean): Hash = {
    append(Value.Boolean.Header)
    append(value)
    complete()
  }

  /**
   * Hashes a number.
   *
   * @param value The number to compute the hash from.
   * @return The hash generated for the specified number.
   */
  private[data] def hashNumber(value: Double): Hash = {
    append(Value.Number.Header)
    append(java.lang.Double.doubleToRawLongBits(value))
    complete()
  }

  /**
   * Hashes a string.
   *
   * @param value The string to compute the hash from.
   * @return The hash generated for the specified string.
   */
  private[data] def hashString(value: String): Hash = {
    append(Value.String.Header)
    append(value)
    complete()
  }

  /**
   * Hashes a create.
   *
   * @param toHash The hash of the document being created to compute the hash from.
   * @return The hash generated for the specified create.
   */
  private[data] def hashCreate(toHash: Hash): Hash = {
    append(Difference.Create.Header)
    append(toHash)
    complete()
  }

  /**
   * Hashes a revise.
   *
   * @param fromHash   The hash of the original document to compute the hash from.
   * @param title      The title of the resulting document to compute the hash from.
   * @param changeHash The hash of the change applied to the original document to compute the hash from.
   * @return The hash generated for the specified revise.
   */
  private[data] def hashRevise(fromHash: Hash, title: String, changeHash: Hash): Hash = {
    append(Difference.Revise.Header)
    append(fromHash)
    append(title)
    append(changeHash)
    complete()
  }

  /**
   * Hashes a delete.
   *
   * @param fromHash The hash of the original document to compute the hash from.
   * @return The hash generated for the specified delete.
   */
  private[data] def hashDelete(fromHash: Hash): Hash = {
    append(Difference.Delete.Header)
    append(fromHash)
    complete()
  }

  /**
   * Hashes an add.
   *
   * @param toHash The hash of the value being added to compute the hash from.
   * @return The hash generated for the specified add.
   */
  private[data] def hashAdd(toHash: Hash): Hash = {
    append(Change.Add.Header)
    append(toHash)
    complete()
  }

  /**
   * Hashes a remove.
   *
   * @param fromHash The hash of the value being removed to compute the hash from.
   * @return The hash generated for the specified remove.
   */
  private[data] def hashRemove(fromHash: Hash): Hash = {
    append(Change.Remove.Header)
    append(fromHash)
    complete()
  }

  /**
   * Hashes a copy.
   *
   * @param fromHash The hash of the value being copied to compute the hash from.
   * @return The hash generated for the specified copy.
   */
  private[data] def hashCopy(fromHash: Hash): Hash = {
    append(Update.Copy.Header)
    append(fromHash)
    complete()
  }

  /**
   * Hashes a replace.
   *
   * @param fromHash The hash of the value being replaced. to compute the hash from
   * @param toHash   The hash of the value doing the replacing to compute the hash from.
   * @return The hash generated for the specified replace.
   */
  private[data] def hashReplace(fromHash: Hash, toHash: Hash): Hash = {
    append(Update.Replace.Header)
    append(fromHash)
    append(toHash)
    complete()
  }

  /**
   * Hashes a modify.
   *
   * @param fromHash     The hash of the original element to compute the hash from.
   * @param changeHashes The flat list of change hashes to compute the hash from.
   * @return The hash generated for the specified modify.
   */
  private[data] def hashModify(fromHash: Hash, changeHashes: Iterable[Hash]): Hash = {
    append(Update.Modify.Header)
    append(fromHash)
    changeHashes foreach append
    complete()
  }

  /**
   * Appends a boolean value to the current digest.
   *
   * @param value The value to append.
   */
  private def append(value: Boolean): Unit =
    digest.update(if (value) 0xFF.toByte else 0x00.toByte)

  /**
   * Appends a byte value to the current digest.
   *
   * @param value The value to append.
   */
  private def append(value: Byte): Unit =
    digest.update(value)

  /**
   * Appends a long value to the current digest.
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
   * Appends a string value to the current digest.
   *
   * @param value The value to append.
   */
  private def append(value: String): Unit =
    digest.update(value.getBytes("UTF-8"))

  /**
   * Appends a hash to the current digest.
   *
   * @param hash The hash to append.
   */
  private def append(hash: Hash): Unit =
    digest.update(internalBytes(hash))

  /**
   * Completes the current digest and generates a hash.
   *
   * @return The resulting hash from the current digest.
   */
  private def complete(): Hash =
    createHash(digest.digest())


}

/**
 * Factory for creating builders.
 */
object Hasher {

  /**
   * Implicitly creates a new hash builder.
   *
   * @return A new hash builder.
   */
  implicit def newImplicitHasher: Hasher = apply()

  /**
   * Creates a new hash builder.
   *
   * @return A new hash builder.
   */
  def apply(): Hasher = new Hasher

}