package net.wayfarerx.dreamsleeve.data

import org.scalatest._

/**
 * Test case for the hashable implementation.
 */
class HashableSpec extends FlatSpec with Matchers {

  "A hashable" should "always return the same hash" in {
    Hashable.hash == Hashable.hash
  }

  /**
   * The hahsable to test
   */
  object Hashable extends Hashable {
    override private[data] def generateHash(implicit hasher: Hasher) =
      Hash(Array[Byte](
        1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
        11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
        21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
        31, 32))
  }

}
