/*
 * Hash.scala
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

/**
 * Represents an immutable SHA-256 hash.
 *
 * @param bytes The 32 bytes that define the hash.
 */
final class Hash private(private val bytes: Array[Byte]) {

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
    if (count < 0 || count > Hash.Size) throw new IllegalArgumentException(count.toString) else {
      val builder = new StringBuilder(count * 2)
      for (i <- 0 until count) {
        builder.append(Hash.Base16(bytes(i) >>> 4 & 0x0F))
        builder.append(Hash.Base16(bytes(i) & 0x0F))
      }
      builder.toString
    }

  /* Encodes all the bytes in base-16. */
  override def toString: String =
    toPrefixString(Hash.Size)

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
    if (bytes.length != Size) throw new IllegalArgumentException(bytes.length.toString) else new Hash(bytes.clone())

  /**
   * Creates a new hash from the specified base-16 textual representation.
   *
   * @param characters The 64 base-16 characters that represent the hash to create.
   * @return A new hash from the specified base-16 textual representation.
   */
  def apply(characters: Array[Char]): Hash =
    apply(java.nio.CharBuffer.wrap(characters))

  /**
   * Creates a new hash from the specified base-16 textual representation.
   *
   * @param characters The 64-character, base-16 string that represents the hash to create.
   * @return A new hash from the specified base-16 textual representation.
   */
  def apply(characters: CharSequence): Hash =
    if (characters.length != Size * 2) throw new IllegalArgumentException(characters.length.toString) else {
      val bytes = new Array[Byte](Size)
      for (i <- 0 until Size) {
        val high = Base16.indexOf(characters.charAt(i * 2).toLower)
        if (high < 0) throw new IllegalArgumentException(characters.charAt(i * 2).toString)
        val low = Base16.indexOf(characters.charAt(i * 2 + 1).toLower)
        if (low < 0) throw new IllegalArgumentException(characters.charAt(i * 2 + 1).toString)
        bytes(i) = ((high << 4 | low) & 0xFF).toByte
      }
      new Hash(bytes)
    }

  /**
   * Creates a new hash without cloning the byte array.
   *
   * @param bytes The bytes to use as the internal byte array for the new hash.
   * @return A new hash without cloning the byte array.
   */
  private[data] def setInternalRepresentation(bytes: Array[Byte]): Hash =
    new Hash(bytes)

  /**
   * Returns the internal byte array of the specified hash.
   *
   * @return The internal byte array of the specified hash.
   */
  private[data] def getInternalRepresentation(hash: Hash): Array[Byte] =
    hash.bytes

}
