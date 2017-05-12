package net.wayfarerx.dreamsleeve.diff

import org.scalatest._

import net.wayfarerx.dreamsleeve.model.{Edit, Hash, Value}

class DifferencesTest extends FlatSpec with Matchers {

  "Differences" should "compute the shortest edit sequence between to tables" in {
    implicit val builder = Hash.Builder()
    val A = Value.String("A")
    val B = Value.String("B")
    val C = Value.String("C")
    Differences(Vector(A, B, C, A, B, B, A), Vector(C, B, A, B, A, C)) shouldBe Vector(
      Edit.Delete(Vector(A.hash, B.hash)),
      Edit.Retain(Vector(C.hash)),
      Edit.Insert(Vector(B)),
      Edit.Retain(Vector(A.hash, B.hash)),
      Edit.Delete(Vector(B.hash)),
      Edit.Retain(Vector(A.hash)),
      Edit.Insert(Vector(C))
    )
  }

}