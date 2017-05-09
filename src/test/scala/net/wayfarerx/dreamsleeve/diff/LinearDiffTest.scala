package net.wayfarerx.dreamsleeve.diff

import org.scalatest._

class LinearDiffTest extends FlatSpec with Matchers {

  import net.wayfarerx.dreamsleeve.model.Edit._
  import net.wayfarerx.dreamsleeve.model.Value

  "The translated linear diff" should "not suck at all" in {
    val from = "ABCABBA"
    val to = "CBABAC"
    var res = LinearDiff.CompareStrings(from, to)
    if (res.getSnakes != null) {
      import scala.collection.JavaConverters._
      for (snake <- res.getSnakes.asScala) {
        System.out.println(snake)
      }
    }
    else System.out.println("No snakes found!")

    val A = Value.String("A")
    val B = Value.String("B")
    val C = Value.String("C")
    LinearDiff.edits(chars(from), chars(to)) shouldBe Vector(
      Remove(Vector(A.hash(), B.hash())),
      Copy(Vector(C.hash())),
      Insert(Vector(B)),
      Copy(Vector(A.hash(), B.hash())),
      Remove(Vector(B.hash())),
      Copy(Vector(A.hash())),
      Insert(Vector(A))
    )
  }

  private def chars(string: String): Vector[Value.String] =
    string.map(c => Value.String(c.toString)).toVector

}