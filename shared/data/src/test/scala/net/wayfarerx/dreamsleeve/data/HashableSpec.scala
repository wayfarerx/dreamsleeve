/*
 * HashableSpec.scala
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

import cats.free.Free.liftF
import org.scalatest._

/**
 * Test case for the hashable support implementation.
 */
class HashableSpec extends FlatSpec with Matchers {

  import Hashable._

  "A hashable" should "always return the same hash from supported components" in {
    TestHashable.hash eq TestHashable.hash shouldBe true
    Hashable.toString
  }

  /**
   * The hashable to test.
   */
  object TestHashable extends Hashable {

    override protected def generateHash(): Hashing[Unit] = for {
      _ <- validate(false)(hashing)
      boolean <- validate(true)(hashing)
      byte <- validate(0x7D.toByte)(hashing)
      short <- validate(0x7D6C.toShort)(hashing)
      char <- validate('x')(hashing)
      int <- validate(0x7D6C5B4A)(hashing)
      float <- validate(Math.PI.toFloat)(hashing)
      long <- validate(0x7D6C5B4A39281706L)(hashing)
      double <- validate(Math.PI)(hashing)
      string <- validate("hello")(hashing)
      collection <- validate(Seq(boolean, byte, short, char, int, float, long, double, string))(hashing)
      hash <- validate(collection)(hashing)
    } yield hashing(hash)

    private def validate[T: TestHashing.Component](c: T)(f: T => Hashing[Unit]): Hashing[Hash] = {
      val x = for {
        _ <- f(c)
        h <- liftF[HashOperation, Hash](d => Hash.setInternalRepresentation(d.digest()))
      } yield h
      val h = x.foldMap(HashOperation(MessageDigest.getInstance("SHA-256")))
      h shouldBe TestHashing(c)
      hashed(h)
    }

  }

}
