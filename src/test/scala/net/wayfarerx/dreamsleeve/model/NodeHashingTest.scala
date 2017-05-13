package net.wayfarerx.dreamsleeve.model

import org.scalatest._
import scala.collection.immutable.ListMap

/**
 * Test case for the node hashing implementation.
 */
class NodeHashingTest extends FlatSpec with Matchers {

  "All nodes" should "produce consistent hashes" in {
    val b = Value.Boolean(true)
    Value.Boolean().hash shouldNot be(b.hash)
    Value.Boolean(true).hash should be(b.hash)
    val n = Value.Number(5)
    b.hash shouldNot be(n.hash)
    Value.Number(42).hash shouldNot be(n.hash)
    Value.Number(5).hash should be(n.hash)
    val s = Value.String("hi")
    s.hash shouldNot be(b.hash)
    s.hash shouldNot be(n.hash)
    Value.String("bye").hash shouldNot be(s.hash)
    Value.String("hi").hash should be(s.hash)
    val t = Table(ListMap(b -> n))
    t.hash shouldNot be(b.hash)
    t.hash shouldNot be(n.hash)
    t.hash shouldNot be(s.hash)
    Table(ListMap(n -> b)).hash shouldNot be(t.hash)
    Table(ListMap(b -> n)).hash should be(t.hash)
  }
  
}