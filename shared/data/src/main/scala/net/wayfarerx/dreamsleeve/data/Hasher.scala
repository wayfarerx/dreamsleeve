/*
 * Hasher.scala
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
 * Utility that generates hashes for a specific set of hash components.
 */
final class Hasher private {

  import Hasher._

  /** The message digest to use. */
  private val digest = MessageDigest.getInstance("SHA-256")

  /**
   * Generates a hash for one hash component.
   *
   * @param c0 The only component to hash.
   * @tparam C0 The type of the only component to hash.
   * @return The hash of the one component.
   */
  def apply[C0: Component](c0: C0): Hash = {
    implicitly[Component[C0]].apply(c0, digest)
    Hash.setInternalRepresentation(digest.digest())
  }

  /**
   * Generates a hash for two hash components.
   *
   * @param c0 The first component to hash.
   * @param c1 The second component to hash.
   * @tparam C0 The type of the first component to hash.
   * @tparam C1 The type of the second component to hash.
   * @return The hash of the two components.
   */
  def apply[C0: Component, C1: Component](c0: C0, c1: C1): Hash = {
    implicitly[Component[C0]].apply(c0, digest)
    implicitly[Component[C1]].apply(c1, digest)
    Hash.setInternalRepresentation(digest.digest())
  }

  /**
   * Generates a hash for three hash components.
   *
   * @param c0 The first component to hash.
   * @param c1 The second component to hash.
   * @param c2 The third component to hash.
   * @tparam C0 The type of the first component to hash.
   * @tparam C1 The type of the second component to hash.
   * @tparam C2 The type of the third component to hash.
   * @return The hash of the three components.
   */
  def apply[C0: Component, C1: Component, C2: Component](c0: C0, c1: C1, c2: C2): Hash = {
    implicitly[Component[C0]].apply(c0, digest)
    implicitly[Component[C1]].apply(c1, digest)
    implicitly[Component[C2]].apply(c2, digest)
    Hash.setInternalRepresentation(digest.digest())
  }

  /**
   * Generates a hash for four hash components.
   *
   * @param c0 The first component to hash.
   * @param c1 The second component to hash.
   * @param c2 The third component to hash.
   * @param c3 The fourth component to hash.
   * @tparam C0 The type of the first component to hash.
   * @tparam C1 The type of the second component to hash.
   * @tparam C2 The type of the third component to hash.
   * @tparam C3 The type of the fourth component to hash.
   * @return The hash of the four components.
   */
  def apply[C0: Component, C1: Component, C2: Component, C3: Component](c0: C0, c1: C1, c2: C2, c3: C3): Hash = {
    implicitly[Component[C0]].apply(c0, digest)
    implicitly[Component[C1]].apply(c1, digest)
    implicitly[Component[C2]].apply(c2, digest)
    implicitly[Component[C3]].apply(c3, digest)
    Hash.setInternalRepresentation(digest.digest())
  }

}

/**
 * Factory for creating hash builders.
 */
object Hasher {

  /**
   * Implicitly creates a new hasher.
   *
   * @return A new hasher.
   */
  implicit def newImplicitHasher: Hasher = apply()

  /**
   * Creates a new hasher.
   *
   * @return A new hasher.
   */
  def apply(): Hasher = new Hasher

  /**
   * Base class for hashed component type classes.
   *
   * @tparam T The type of the underlying hashed component.
   */
  sealed trait Component[-T] {

    /**
     * Appends the specified hashed component to a message digest.
     *
     * @param component The hashed component to append.
     * @param digest    The message digest to append to.
     */
    private[Hasher] def apply(component: T, digest: MessageDigest): Unit

  }

  /**
   * Implementations of the supported hashed components.
   */
  object Component {

    /** Support for booleans as hashed components. */
    implicit val Booleans: Component[Boolean] = new Component[Boolean] {
      override private[Hasher] def apply(c: Boolean, d: MessageDigest): Unit =
        d.update(if (c) 0xFF.toByte else 0x00.toByte)
    }

    /** Support for bytes as hashed components. */
    implicit val Bytes: Component[Byte] = new Component[Byte] {
      override private[Hasher] def apply(c: Byte, d: MessageDigest): Unit =
        d.update(c)
    }

    /** Support for shorts as hashed components. */
    implicit val Shorts: Component[Short] = new Component[Short] {
      override private[Hasher] def apply(c: Short, d: MessageDigest): Unit = {
        d.update((c >>> 8 & 0x00FF).toByte)
        d.update((c & 0x00FF).toByte)
      }
    }

    /** Support for chars as hashed components. */
    implicit val Chars: Component[Char] = new Component[Char] {
      override private[Hasher] def apply(c: Char, d: MessageDigest): Unit = {
        d.update((c >>> 8 & 0x00FF).toByte)
        d.update((c & 0x00FF).toByte)
      }
    }

    /** Support for ints as hashed components. */
    implicit val Ints: Component[Int] = new Component[Int] {
      override private[Hasher] def apply(c: Int, d: MessageDigest): Unit = {
        d.update((c >>> 24 & 0x000000FF).toByte)
        d.update((c >>> 16 & 0x000000FF).toByte)
        d.update((c >>> 8 & 0x000000FF).toByte)
        d.update((c & 0x000000FF).toByte)
      }
    }

    /** Support for floats as hashed components. */
    implicit val Floats: Component[Float] = new Component[Float] {
      override private[Hasher] def apply(c: Float, d: MessageDigest): Unit =
        Ints(java.lang.Float.floatToIntBits(c), d)
    }

    /** Support for longs as hashed components. */
    implicit val Longs: Component[Long] = new Component[Long] {
      override private[Hasher] def apply(c: Long, d: MessageDigest): Unit = {
        d.update((c >>> 56 & 0x00000000000000FF).toByte)
        d.update((c >>> 48 & 0x00000000000000FF).toByte)
        d.update((c >>> 40 & 0x00000000000000FF).toByte)
        d.update((c >>> 32 & 0x00000000000000FF).toByte)
        d.update((c >>> 24 & 0x00000000000000FF).toByte)
        d.update((c >>> 16 & 0x00000000000000FF).toByte)
        d.update((c >>> 8 & 0x00000000000000FF).toByte)
        d.update((c & 0x00000000000000FF).toByte)
      }
    }

    /** Support for doubles as hashed components. */
    implicit val Doubles: Component[Double] = new Component[Double] {
      override private[Hasher] def apply(c: Double, d: MessageDigest): Unit =
        Longs(java.lang.Double.doubleToLongBits(c), d)
    }

    /** Support for strings as hashed components. */
    implicit val Strings: Component[String] = new Component[String] {
      override private[Hasher] def apply(c: String, d: MessageDigest): Unit =
        d.update(c.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    }

    /** Support for hashes as hashed components. */
    implicit val Hashes: Component[Hash] = new Component[Hash] {
      override private[Hasher] def apply(c: Hash, d: MessageDigest): Unit =
        d.update(Hash.getInternalRepresentation(c))
    }

    /**
     * Support for collections of hashed components as hashed components.
     *
     * @tparam T The type of hashed component being collected.
     * @return Support for collections of hashed components as hashed components.
     */
    implicit def collections[T: Component]: Component[Iterable[T]] = new Component[Iterable[T]] {
      override private[Hasher] def apply(c: Iterable[T], d: MessageDigest): Unit =
        for (i <- c) implicitly[Component[T]].apply(i, d)
    }

  }

}
