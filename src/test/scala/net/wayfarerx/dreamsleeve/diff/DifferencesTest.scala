package net.wayfarerx.dreamsleeve.diff

import org.scalatest._

import net.wayfarerx.dreamsleeve.model.Value

/**
 * Test case for the value diffing implementation.
 */
class DifferencesTest extends FlatSpec with Matchers {

  "Differences" should "compute the shortest edit sequence between to sequences of values" in {
    val A = Value.String("A")
    val B = Value.String("B")
    val C = Value.String("C")
    Differences(Vector(A, B, C, A, B, B, A), Vector(C, B, A, B, A, C)) shouldBe Vector(
      Differences.Delete(Vector(A, B)),
      Differences.Retain(Vector(C)),
      Differences.Insert(Vector(B)),
      Differences.Retain(Vector(A, B)),
      Differences.Delete(Vector(B)),
      Differences.Retain(Vector(A)),
      Differences.Insert(Vector(C))
    )
  }

}