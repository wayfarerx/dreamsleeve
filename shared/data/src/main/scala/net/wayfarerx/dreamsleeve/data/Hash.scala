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

import java.security.MessageDigest

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

  /** The implementation of equality for hashes. */
  implicit val Eq: cats.Eq[Hash] = (x: Hash, y: Hash) => x == y

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
   * Returns the internal byte array of the specified hash.
   *
   * @return The internal byte array of the specified hash.
   */
  private[data] def getInternalRepresentation(hash: Hash): Array[Byte] =
    hash.bytes

  /**
   * Creates a new hash without cloning the byte array.
   *
   * @param bytes The bytes to use as the internal byte array for the new hash.
   * @return A new hash without cloning the byte array.
   */
  private[data] def setInternalRepresentation(bytes: Array[Byte]): Hash =
    new Hash(bytes)

  /**
   * A utility to assist with generating hashes.
   */
  trait Generator {

    import Generator._

    /**
     * Returns the underlying message digest.
     *
     * @return The underlying message digest.
     */
    def digest: MessageDigest

    /**
     * Generates a hash for one hash component.
     *
     * @param a The only component to hash.
     * @tparam A The type of the only component to hash.
     * @return The hash of the one component.
     */
    def hash[A: Component](a: A): Hash = {
      implicitly[Component[A]].apply(a, digest)
      Hash.setInternalRepresentation(digest.digest())
    }

    /**
     * Generates a hash for two hash components.
     *
     * @param a The first component to hash.
     * @param b The second component to hash.
     * @tparam A The type of the first component to hash.
     * @tparam B The type of the second component to hash.
     * @return The hash of the two components.
     */
    def hash[A: Component, B: Component](a: A, b: B): Hash = {
      implicitly[Component[A]].apply(a, digest)
      implicitly[Component[B]].apply(b, digest)
      Hash.setInternalRepresentation(digest.digest())
    }

    /**
     * Generates a hash for three hash components.
     *
     * @param a The first component to hash.
     * @param b The second component to hash.
     * @param c The third component to hash.
     * @tparam A The type of the first component to hash.
     * @tparam B The type of the second component to hash.
     * @tparam C The type of the third component to hash.
     * @return The hash of the three components.
     */
    def hash[A: Component, B: Component, C: Component](a: A, b: B, c: C): Hash = {
      implicitly[Component[A]].apply(a, digest)
      implicitly[Component[B]].apply(b, digest)
      implicitly[Component[C]].apply(c, digest)
      Hash.setInternalRepresentation(digest.digest())
    }

    /**
     * Generates a hash for four hash components.
     *
     * @param a The first component to hash.
     * @param b The second component to hash.
     * @param c The third component to hash.
     * @param d The fourth component to hash.
     * @tparam A The type of the first component to hash.
     * @tparam B The type of the second component to hash.
     * @tparam C The type of the third component to hash.
     * @tparam D The type of the fourth component to hash.
     * @return The hash of the four components.
     */
    def hash[A: Component, B: Component, C: Component, D: Component](a: A, b: B, c: C, d: D): Hash = {
      implicitly[Component[A]].apply(a, digest)
      implicitly[Component[B]].apply(b, digest)
      implicitly[Component[C]].apply(c, digest)
      implicitly[Component[D]].apply(d, digest)
      Hash.setInternalRepresentation(digest.digest())
    }

  }

  /**
   * Definitions associated with hash generators.
   */
  object Generator {

    /**
     * Creates a new hash generator for the specified message digest, defaulting to SHA-256.
     *
     * @param digest The message digest to use.
     * @return A new hash generator for the specified message digest, defaulting to SHA-256.
     */
    def apply(digest: MessageDigest = MessageDigest.getInstance("SHA-256")): Generator = {
      val _digest = digest
      new Generator {
        override def digest: MessageDigest = _digest
      }
    }

    /**
     * Base class for hash component type classes.
     *
     * @tparam T The type of the underlying hash component.
     */
    sealed trait Component[-T] {

      /**
       * Appends the specified hash component to a message digest.
       *
       * @param component The hash component to append.
       * @param digest    The message digest to append to.
       */
      def apply(component: T, digest: MessageDigest): Unit

    }

    /**
     * Implementations of the supported hash components.
     */
    object Component {

      /** Support for booleans as hash components. */
      implicit val Booleans: Component[Boolean] = new Component[Boolean] {
        override def apply(i: Boolean, d: MessageDigest): Unit =
          d.update(if (i) 0xFF.toByte else 0x00.toByte)
      }

      /** Support for bytes as hash components. */
      implicit val Bytes: Component[Byte] = new Component[Byte] {
        override def apply(i: Byte, d: MessageDigest): Unit =
          d.update(i)
      }

      /** Support for longs as hash components. */
      implicit val Longs: Component[Long] = new Component[Long] {
        override def apply(i: Long, d: MessageDigest): Unit = {
          d.update((i >>> 56 & 0x00000000000000FF).toByte)
          d.update((i >>> 48 & 0x00000000000000FF).toByte)
          d.update((i >>> 40 & 0x00000000000000FF).toByte)
          d.update((i >>> 32 & 0x00000000000000FF).toByte)
          d.update((i >>> 24 & 0x00000000000000FF).toByte)
          d.update((i >>> 16 & 0x00000000000000FF).toByte)
          d.update((i >>> 8 & 0x00000000000000FF).toByte)
          d.update((i & 0x00000000000000FF).toByte)
        }
      }

      /** Support for doubles as hash components. */
      implicit val Doubles: Component[Double] = new Component[Double] {
        override def apply(i: Double, d: MessageDigest): Unit =
          Longs(java.lang.Double.doubleToLongBits(i), d)
      }

      /** Support for strings as hash components. */
      implicit val Strings: Component[String] = new Component[String] {
        override def apply(i: String, d: MessageDigest): Unit =
          d.update(i.getBytes(java.nio.charset.StandardCharsets.UTF_8))
      }

      /** Support for hashes as hash components. */
      implicit val Hashes: Component[Hash] = new Component[Hash] {
        override def apply(i: Hash, d: MessageDigest): Unit =
          d.update(Hash.getInternalRepresentation(i))
      }

      /** Support for collections of hashes as hash components. */
      implicit val MultipleHashes: Component[Iterable[Hash]] = new Component[Iterable[Hash]] {
        override def apply(i: Iterable[Hash], d: MessageDigest): Unit =
          for (i <- i) Hashes.apply(i, d)
      }

    }

  }

}
