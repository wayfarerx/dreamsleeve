package net.wayfarerx.dreamsleeve.diff

import org.scalatest._

import net.wayfarerx.dreamsleeve.model.Edit._
import net.wayfarerx.dreamsleeve.model.Value

class LinearDiffTest extends FlatSpec with Matchers {

  "The translated linear diff" should "not suck at all" in {
    val A = Value.String("A")
    val B = Value.String("B")
    val C = Value.String("C")
    LinearDiff.edits(Vector(A, B, C, A, B, B, A), Vector(C, B, A, B, A, C)) shouldBe Vector(
      Remove(Vector(A.hash(), B.hash())),
      Copy(Vector(C.hash())),
      Insert(Vector(B)),
      Copy(Vector(A.hash(), B.hash())),
      Remove(Vector(B.hash())),
      Copy(Vector(A.hash())),
      Insert(Vector(C))
    )
  }

}