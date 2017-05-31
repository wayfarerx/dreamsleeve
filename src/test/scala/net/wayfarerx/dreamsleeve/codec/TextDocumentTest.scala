package net.wayfarerx.dreamsleeve.codec

import org.scalatest._

import net.wayfarerx.dreamsleeve.model.Node

/**
 * Test case for the document parsing implementation.
 */
class TextDocumentTest extends FlatSpec with Matchers {

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

  "TextDocument" should "decode and encode documents" in {
    val a = TextDocument.readString(A)
    a.title should be("MM00DataSavedVariables")
    val at = TextDocument.writeString(a)
    val b = TextDocument.readString(at)
    a shouldBe b
  }

  it should "decode and encode master merchant files" in {
    val a = parse("../MM00Data-0.lua")
    a.title should be("MM00DataSavedVariables")
    val at = TextDocument.writeString(a)
    val b = TextDocument.readString(at)
    a shouldBe b
  }

  /**
   * Parses the specified document.
   *
   * @param document The relative path of the document to parse.
   * @return The document parser.
   */
  private def parse(document: String): Node.Document = {
    val input = getClass.getResourceAsStream(document)
    try TextDocument.read(input) finally input.close()
  }

}
