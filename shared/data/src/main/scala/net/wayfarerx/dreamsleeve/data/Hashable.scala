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

import cats.{Id, ~>}
import cats.free.Free
import Free.liftF

/**
 * Base type for data elements that can be hashed.
 */
abstract class Hashable {

  import Hashable._

  /** The language that describes hashing operations. */
  final type Hashing[T] = Hashable.Hashing[T]

  /** The cached hash of this data element. */
  private var _hash: Option[Hash] = None

  /**
   * Returns the hash for this data element, generating it if necessary.
   *
   * @return The hash for this data element.
   */
  final def hash: Hash =
    _hash getOrElse hashed.foldMap(HashOperation(MessageDigest.getInstance("SHA-256")))

  /**
   * Creates an operation that can calculate the hash of this data element.
   *
   * @return An operation that can calculate the hash of this data element.
   */
  final def hashed: Hashing[Hash] = for {
    c <- liftF[HashOperation, Option[Hash]](_ => _hash)
    r <- c map Free.pure[HashOperation, Hash] getOrElse {
      for {
        _ <- generateHash()
        h <- liftF[HashOperation, Hash](d => Hash.setInternalRepresentation(d.digest()))
      } yield {
        _hash = Some(h)
        h
      }
    }
  } yield r

  /**
   * Generates the hash for this data element.
   *
   * @return The hash for this data element.
   */
  protected def generateHash(): Hashing[Unit]

  /**
   * Creates a hasher that returns the specified object.
   *
   * @param result The object to return.
   * @tparam T The type of object to return.
   * @return A hasher that returns the specified object.
   */
  final protected def hashed[T](result: T): Hashing[T] =
    Free.pure(result)

  /**
   * Creates a hasher for the specified component.
   *
   * @param v The component to create a hasher for.
   * @return A hasher for the specified component.
   */
  final protected def hashing(v: Boolean): Hashing[Unit] =
    liftF[HashOperation, Unit](_.update(if (v) 0xFF.toByte else 0x00.toByte))

  /**
   * Creates a hasher for the specified component.
   *
   * @param v The component to create a hasher for.
   * @return A hasher for the specified component.
   */
  final protected def hashing(v: Byte): Hashing[Unit] =
    liftF[HashOperation, Unit](_.update(v))

  /**
   * Creates a hasher for the specified component.
   *
   * @param v The component to create a hasher for.
   * @return A hasher for the specified component.
   */
  final protected def hashing(v: Short): Hashing[Unit] =
    liftF[HashOperation, Unit] { d =>
      d.update((v >>> 8 & 0x00FF).toByte)
      d.update((v & 0x00FF).toByte)
    }

  /**
   * Creates a hasher for the specified component.
   *
   * @param v The component to create a hasher for.
   * @return A hasher for the specified component.
   */
  final protected def hashing(v: Char): Hashing[Unit] =
    liftF[HashOperation, Unit] { d =>
      d.update((v >>> 8 & 0x00FF).toByte)
      d.update((v & 0x00FF).toByte)
    }

  /**
   * Creates a hasher for the specified component.
   *
   * @param v The component to create a hasher for.
   * @return A hasher for the specified component.
   */
  final protected def hashing(v: Int): Hashing[Unit] =
    liftF[HashOperation, Unit] { d =>
      d.update((v >>> 24 & 0x000000FF).toByte)
      d.update((v >>> 16 & 0x000000FF).toByte)
      d.update((v >>> 8 & 0x000000FF).toByte)
      d.update((v & 0x000000FF).toByte)
    }

  /**
   * Creates a hasher for the specified component.
   *
   * @param v The component to create a hasher for.
   * @return A hasher for the specified component.
   */
  final protected def hashing(v: Float): Hashing[Unit] = for {
    r <- hashing(java.lang.Float.floatToIntBits(v))
  } yield r

  /**
   * Creates a hasher for the specified component.
   *
   * @param v The component to create a hasher for.
   * @return A hasher for the specified component.
   */
  final protected def hashing(v: Long): Hashing[Unit] =
    liftF[HashOperation, Unit] { d =>
      d.update((v >>> 56 & 0x00000000000000FF).toByte)
      d.update((v >>> 48 & 0x00000000000000FF).toByte)
      d.update((v >>> 40 & 0x00000000000000FF).toByte)
      d.update((v >>> 32 & 0x00000000000000FF).toByte)
      d.update((v >>> 24 & 0x00000000000000FF).toByte)
      d.update((v >>> 16 & 0x00000000000000FF).toByte)
      d.update((v >>> 8 & 0x00000000000000FF).toByte)
      d.update((v & 0x00000000000000FF).toByte)
    }

  /**
   * Creates a hasher for the specified component.
   *
   * @param v The component to create a hasher for.
   * @return A hasher for the specified component.
   */
  final protected def hashing(v: Double): Hashing[Unit] = for {
    r <- hashing(java.lang.Double.doubleToLongBits(v))
  } yield r

  /**
   * Creates a hasher for the specified component.
   *
   * @param v The component to create a hasher for.
   * @return A hasher for the specified component.
   */
  final protected def hashing(v: String): Hashing[Unit] =
    liftF[HashOperation, Unit](_.update(v.getBytes(java.nio.charset.StandardCharsets.UTF_8)))

  /**
   * Creates a hasher for the specified component.
   *
   * @param v The component to create a hasher for.
   * @return A hasher for the specified component.
   */
  final protected def hashing(v: Hash): Hashing[Unit] =
    liftF[HashOperation, Unit](_.update(Hash.getInternalRepresentation(v)))

  /**
   * Creates a hasher for the specified component.
   *
   * @param v The component to create a hasher for.
   * @return A hasher for the specified component.
   */
  final protected def hashing(v: Iterable[Hash]): Hashing[Unit] =
    liftF[HashOperation, Unit](d => v foreach (h => d.update(Hash.getInternalRepresentation(h))))

}

/**
 * Types associated with data elements that can be hashed.
 */
object Hashable {

  /** The language that describes hashing operations. */
  type Hashing[T] = Free[HashOperation, T]

  /**
   * Base class for operation that hash data.
   *
   * @tparam R The type returned by this operation.
   */
  trait HashOperation[R] {

    /**
     * Applies this operation.
     *
     * @param digest The message digest to use.
     * @return The result of this hashing operation.
     */
    def apply(digest: MessageDigest): R

  }

  /**
   * Declarations associated with hashing operations.
   */
  object HashOperation {

    /**
     * Creates an interpreter for patching operations.
     *
     * @param digest The message digest to use.
     * @return An interpreter for patching operations.
     */
    def apply(digest: MessageDigest): HashOperation ~> Id = new (HashOperation ~> Id) {
      override def apply[R](op: HashOperation[R]): Id[R] = op(digest)
    }

  }

}