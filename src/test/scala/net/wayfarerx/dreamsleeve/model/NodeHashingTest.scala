package net.wayfarerx.dreamsleeve.model

import org.scalatest._

class NodeHashingTest extends FlatSpec with Matchers {

  "All nodes" should "produce consistent hashes" in {
    val b = Value.Boolean(true)
    Value.Boolean(false).hash shouldNot be (b.hash)
    val n = Value.Number(5)
    b.hash shouldNot be (n.hash)
    Value.Number(42).hash shouldNot be (n.hash)
  }
  
}