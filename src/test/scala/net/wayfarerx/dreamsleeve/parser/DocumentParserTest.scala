package net.wayfarerx.dreamsleeve.parser

import org.parboiled2.ParserInput
import org.scalatest._

/**
 * Created by wayfarerx on 4/28/17.
 */
class DocumentParserTest extends FlatSpec with Matchers {

  "DocumentParser" should "parse master merchant files" in {
    val resource = getClass.getResource("MasterMerchant.lua")
    val connection = resource.openConnection()
    val data = new Array[Byte](connection.getContentLength)
    val input = connection.getInputStream
    try input.read(data) finally input.close()
    val parser = new DocumentParser(ParserInput(data))
    val result = parser.Document.run()
    result.isSuccess should be (true)
  }

}
