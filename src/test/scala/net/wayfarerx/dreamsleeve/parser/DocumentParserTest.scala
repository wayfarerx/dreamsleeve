package net.wayfarerx.dreamsleeve.parser

import org.parboiled2.ParserInput
import org.scalatest._

/**
 * Test case for the document parsing implementation.
 */
class DocumentParserTest extends FlatSpec with Matchers {

  "DocumentParser" should "parse master merchant files" in {
    parse("MasterMerchant.lua").Document.run().isSuccess should be (true)
  }

  /**
   * Parses the specified document.
   *
   * @param document The relative path of the document to parse.
   * @return The document parser.
   */
  private def parse(document: String): DocumentParser = {
    val resource = getClass.getResource(document)
    val connection = resource.openConnection()
    val data = new Array[Byte](connection.getContentLength)
    val input = connection.getInputStream
    try input.read(data) finally input.close()
    new DocumentParser(ParserInput(data))
  }

}
