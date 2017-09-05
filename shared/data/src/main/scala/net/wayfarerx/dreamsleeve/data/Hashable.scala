/*
 * Hashable.scala
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

import cats._
import free.Free
import Free.liftF

/**
 * Base type for hashable elements.
 */
abstract class Hashable {

  /** Alias for the language that describes hashing operations. */
  final type HashOperation[T] = Hashable.HashOperation[T]

  /** Alias for the tasks that make up hashing operations. */
  final protected type HashTask[T] = Hashable.HashTask[T]

  /** The cached hash of this element. */
  private var _hash: Option[Hash] = None

  /**
   * Returns the hash for this element, generating it if necessary.
   *
   * @return The hash for this element.
   */
  final def hash: Hash =
    _hash getOrElse hashOperation.foldMap(HashTask.interpreter())

  /**
   * Creates an operation that can calculate the hash of this element.
   *
   * @return An operation that can calculate the hash of this element.
   */
  final def hashOperation: HashOperation[Hash] = for {
    r <- _hash map HashTask.pure getOrElse {
      for (h <- calculateHash()) yield {
        _hash = Some(h)
        h
      }
    }
  } yield r

  /**
   * Calculates the hash for this element.
   *
   * @return The hash for this element.
   */
  protected def calculateHash(): HashOperation[Hash]

  /**
   * Alias for the hash task factory.
   *
   * @return The alias for the hash task factory
   */
  final protected def HashTask: Hashable.HashTask.type = Hashable.HashTask

}

/**
 * Types associated with hashable elements.
 */
object Hashable {

  /** The language that describes hashing operations. */
  type HashOperation[T] = Free[HashTask, T]

  /**
   * Base class for tasks that hash data.
   *
   * @tparam T The type returned by this task.
   */
  sealed trait HashTask[T] {

    /**
     * Applies this task.
     *
     * @param digest The message digest to use.
     * @return The result of this hashing task.
     */
    def apply(digest: MessageDigest): T

  }

  /**
   * Declarations associated with hashing tasks.
   */
  object HashTask {

    /**
     * Creates a hasher that returns the specified object.
     *
     * @param result The object to return.
     * @tparam T The type of object to return.
     * @return A hasher that returns the specified object.
     */
    def pure[T](result: T): HashOperation[T] =
      Free.pure[HashTask, T](result)

    /**
     * Generates a hash for one hash item.
     *
     * @param a The only item to hash.
     * @tparam A The type of the only item to hash.
     * @return The hash of the one item.
     */
    def hash[A: Item](a: A): HashOperation[Hash] =
      liftF[HashTask, Hash](new HashTask[Hash] {
        override def apply(digest: MessageDigest): Hash = {
          implicitly[Item[A]].apply(a, digest)
          Hash.setInternalRepresentation(digest.digest())
        }
      })

    /**
     * Generates a hash for two hash items.
     *
     * @param a The first item to hash.
     * @param b The second item to hash.
     * @tparam A The type of the first item to hash.
     * @tparam B The type of the second item to hash.
     * @return The hash of the two items.
     */
    def hash[A: Item, B: Item](a: A, b: B): HashOperation[Hash] =
      liftF[HashTask, Hash](new HashTask[Hash] {
        override def apply(digest: MessageDigest): Hash = {
          implicitly[Item[A]].apply(a, digest)
          implicitly[Item[B]].apply(b, digest)
          Hash.setInternalRepresentation(digest.digest())
        }
      })

    /**
     * Generates a hash for three hash items.
     *
     * @param a The first item to hash.
     * @param b The second item to hash.
     * @param c The third item to hash.
     * @tparam A The type of the first item to hash.
     * @tparam B The type of the second item to hash.
     * @tparam C The type of the third item to hash.
     * @return The hash of the three items.
     */
    def hash[A: Item, B: Item, C: Item](a: A, b: B, c: C): HashOperation[Hash] =
      liftF[HashTask, Hash](new HashTask[Hash] {
        override def apply(digest: MessageDigest): Hash = {
          implicitly[Item[A]].apply(a, digest)
          implicitly[Item[B]].apply(b, digest)
          implicitly[Item[C]].apply(c, digest)
          Hash.setInternalRepresentation(digest.digest())
        }
      })

    /**
     * Generates a hash for four hash items.
     *
     * @param a The first item to hash.
     * @param b The second item to hash.
     * @param c The third item to hash.
     * @param d The fourth item to hash.
     * @tparam A The type of the first item to hash.
     * @tparam B The type of the second item to hash.
     * @tparam C The type of the third item to hash.
     * @tparam D The type of the fourth item to hash.
     * @return The hash of the four items.
     */
    def hash[A: Item, B: Item, C: Item, D: Item](a: A, b: B, c: C, d: D): HashOperation[Hash] =
      liftF[HashTask, Hash](new HashTask[Hash] {
        override def apply(digest: MessageDigest): Hash = {
          implicitly[Item[A]].apply(a, digest)
          implicitly[Item[B]].apply(b, digest)
          implicitly[Item[C]].apply(c, digest)
          implicitly[Item[D]].apply(d, digest)
          Hash.setInternalRepresentation(digest.digest())
        }
      })

    /**
     * Creates an interpreter for hashing tasks.
     *
     * @param digest The message digest to use.
     * @return An interpreter for hashing tasks.
     */
    def interpreter(digest: MessageDigest = MessageDigest.getInstance("SHA-256")): HashTask ~> Id =
      new (HashTask ~> Id) {
        override def apply[R](op: HashTask[R]): Id[R] = op(digest)
      }

  }

  /**
   * Base class for hash item type classes.
   *
   * @tparam T The type of the underlying hash item.
   */
  sealed trait Item[-T] {

    /**
     * Appends the specified hash item to a message digest.
     *
     * @param item   The hash item to append.
     * @param digest The message digest to append to.
     */
    def apply(item: T, digest: MessageDigest): Unit

  }

  /**
   * Implementations of the supported hash items.
   */
  object Item {

    /** Support for booleans as hash items. */
    implicit val Booleans: Item[Boolean] = new Item[Boolean] {
      override def apply(i: Boolean, d: MessageDigest): Unit =
        d.update(if (i) 0xFF.toByte else 0x00.toByte)
    }

    /** Support for bytes as hash items. */
    implicit val Bytes: Item[Byte] = new Item[Byte] {
      override def apply(i: Byte, d: MessageDigest): Unit =
        d.update(i)
    }

    /** Support for longs as hash items. */
    implicit val Longs: Item[Long] = new Item[Long] {
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

    /** Support for doubles as hash items. */
    implicit val Doubles: Item[Double] = new Item[Double] {
      override def apply(i: Double, d: MessageDigest): Unit =
        Longs(java.lang.Double.doubleToLongBits(i), d)
    }

    /** Support for strings as hash items. */
    implicit val Strings: Item[String] = new Item[String] {
      override def apply(i: String, d: MessageDigest): Unit =
        d.update(i.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    }

    /** Support for hashes as hash items. */
    implicit val Hashes: Item[Hash] = new Item[Hash] {
      override def apply(i: Hash, d: MessageDigest): Unit =
        d.update(Hash.getInternalRepresentation(i))
    }

    /** Support for collections of hashes as hash items. */
    implicit val MultipleHashes: Item[Iterable[Hash]] = new Item[Iterable[Hash]] {
      override def apply(i: Iterable[Hash], d: MessageDigest): Unit =
        for (i <- i) Hashes.apply(i, d)
    }

  }

}
