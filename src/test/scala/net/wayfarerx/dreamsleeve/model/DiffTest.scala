package net.wayfarerx.dreamsleeve.model

import org.scalatest._

import net.wayfarerx.dreamsleeve.codec.TextDocument

/**
 * Test case for the node hashing implementation.
 */
class DiffTest extends FlatSpec with Matchers {

  import Node._

  val A =
    """
      MM00DataSavedVariables =
      {
        [2] =
        {
            ["timestamp"] = 1494658163,
            ["seller"] = "@ua02047",
            ["buyer"] = "@zekak",
            ["itemLink"] = "|H0:item:43009:362:50:0:0:0:0:0:0:0:0:0:0:0:0:3:0:0:0:0:0|h|h",
            ["id"] = "755754105",
            ["guild"] = "Iron Bank of Bravos",
            ["quant"] = 1,
            ["wasKiosk"] = true,
            ["price"] = 399,
        }
      }
    """
  val B =
    """
      MM00DataSavedVariables =
      {
        [2] =
        {
            ["wasKiosk"] = true,
            ["buyer"] = "@zekak",
            ["guild"] = "Iron Bank of Bravos",
            ["itemLink"] = "|H0:item:43009:362:50:0:0:0:0:0:0:0:0:0:0:0:0:3:0:0:0:0:0|h|h",
            ["seller"] = "@ua02047",
            ["quant"] = 1,
            ["price"] = 399,
            ["timestamp"] = 1494658163,
            ["id"] = "755754105",
        }
      }
    """

  "All nodes" should "create and apply diffs" in {
    val a = TextDocument.readString(A)
    Diff.Revise(a, a).apply(a) shouldBe a
    val b = TextDocument.readString(B)
    Diff.Revise(a, b).apply(a) shouldBe b
  }

  it should "create and apply diffs from big files" in {
    val a = loadFrom("../MM00Data-0.lua")
    Diff.Revise(a, a).apply(a) shouldBe a
    val b = loadFrom("../MM00Data-1.lua")
    Diff.Revise(a, b).apply(a) shouldBe b
  }

  /** Loads a document from a file. */
  def loadFrom(file: String): Document = {
    val input = getClass.getResourceAsStream(file)
    try TextDocument.read(input) finally input.close()
  }

}