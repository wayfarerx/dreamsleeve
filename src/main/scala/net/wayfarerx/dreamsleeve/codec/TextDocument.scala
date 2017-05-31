package net.wayfarerx.dreamsleeve.codec

import java.io._

import org.parboiled2._

import net.wayfarerx.dreamsleeve.model.Node

/**
 * The codec for reading and writing documents as text.
 */
object TextDocument {

  import Node._

  /*
   * Characters that are escaped when writing.
   */
  private val BL = 0x00000007.toChar
  private val BK = '\b'
  private val CR = '\r'
  private val NL = '\n'
  private val FF = '\f'
  private val HT = '\t'
  private val VT = 0x0000000B.toChar
  private val BS = '\\'
  private val SQ = '\''
  private val DQ = '"'
  private val LB = '['
  private val RB = ']'

  /** The string to use for a single indentation. */
  private val Indent = " " * 4
  /** The string to use for line breaks. */
  private val LineBreak = System.getProperty("line.separator")

  /**
   * Writes a document to a UTF-8 byte array.
   *
   * @param document The document to write.
   * @return The UTF-8 bytes of the written document.
   */
  def writeBytes(document: Document): Array[Byte] = {
    val output = new ByteArrayOutputStream
    write(document, output)
    output.toByteArray
  }

  /**
   * Writes a document to a string.
   *
   * @param document The document to write.
   * @return The string of the written document.
   */
  def writeString(document: Document): String = {
    val writer = new StringWriter
    write(document, writer)
    writer.toString
  }

  /**
   * Writes a document to an output stream as UTF-8 bytes.
   *
   * @param document The document to write.
   * @param output   The output stream to write the document to as UTF-8 bytes.
   */
  def write(document: Document, output: OutputStream): Unit = {
    val writer = new OutputStreamWriter(output, "UTF-8")
    write(document, writer)
    writer.flush()
  }

  /**
   * Writes a document to a writer.
   *
   * @param document The document to write.
   * @param writer   The writer to write the document to.
   */
  def write(document: Document, writer: Writer): Unit = {
    val buffer = new StringBuilder

    /* Encodes a number for writing. */
    def encodeNumber(input: Double): String =
      if ((input == Math.floor(input)) && !java.lang.Double.isInfinite(input)) input.toLong.toString
      else input.toString

    /* Encodes a string for writing. */
    def encodeString(input: String): String = {
      buffer.clear()
      for (c <- input) c match {
        case BL => buffer.append("""\a""")
        case BK => buffer.append("""\b""")
        case CR => buffer.append("""\r""")
        case NL => buffer.append("""\n""")
        case FF => buffer.append("""\f""")
        case HT => buffer.append("""\t""")
        case VT => buffer.append("""\v""")
        case BS => buffer.append("""\\""")
        case SQ => buffer.append("""\'""")
        case DQ => buffer.append("""\"""")
        case LB => buffer.append("""\[""")
        case RB => buffer.append("""\]""")
        case _ => buffer.append(c)
      }
      buffer.toString
    }

    /* Writes a data node at the specified depth. */
    def write(data: Data, depth: Int): Unit = data match {
      case Table(entries) =>
        writer.write(LineBreak)
        writer.write(Indent * depth)
        writer.write('{')
        writer.write(LineBreak)
        val deeper = depth + 1
        for ((k, v) <- entries) {
          writer.write(Indent * deeper)
          writer.write(LB)
          write(k, deeper)
          writer.write(RB)
          writer.write(" = ")
          write(v, deeper)
          writer.write(',')
          writer.write(LineBreak)
        }
        writer.write(Indent * depth)
        writer.write('}')
      case Value.String(s) =>
        writer.write(DQ)
        writer.write(encodeString(s))
        writer.write(DQ)
      case Value.Number(n) =>
        writer.write(encodeNumber(n))
      case Value.Boolean(b) =>
        writer.write(b.toString)
    }

    writer.write(document.title)
    writer.write(" = ")
    write(document.content, 0)
    writer.write(LineBreak)
  }

  /**
   * Reads a document from a UTF-8 byte array.
   *
   * @param input The UTF-8 bytes to read a document from.
   * @return The document read from the specified bytes.
   */
  def readBytes(input: Array[Byte]): Document =
    new DocumentParser(ParserInput(new String(input, "UTF-8"))).DocumentNode.run().get

  /**
   * Reads a document from a string.
   *
   * @param input The string to read a document from.
   * @return The document read from the specified string.
   */
  def readString(input: String): Document =
    new DocumentParser(ParserInput(input)).DocumentNode.run().get

  /**
   * Reads a document from a UTF-8 input stream.
   *
   * @param input The input stream to read a document from in UTF-8.
   * @return The document read from the specified UTF-8 input stream.
   */
  def read(input: InputStream): Document =
    read(new InputStreamReader(input, "UTF-8"))

  /**
   * Reads a document from a reader.
   *
   * @param reader The reader to read a document from.
   * @return The document read from the specified reader.
   */
  def read(reader: Reader): Document = {
    val builder = new StringBuilder
    val buffer = new Array[Char](512)
    var count = reader.read(buffer)
    while (count >= 0) {
      builder.appendAll(buffer, 0, count)
      count = reader.read(buffer)
    }
    new DocumentParser(ParserInput(builder.toString)).DocumentNode.run().get
  }

  /**
   * A parser for documents written to text files.
   *
   * @param input The input to this parser.
   */
  private class DocumentParser(override val input: ParserInput) extends Parser {

    //
    // Rules that define the syntax of document nodes.
    //

    def DocumentNode: Rule1[Document] = rule {
      Whitespace ~ DocumentTitle ~ Equals ~ DataNode ~ EOI ~> { (t: String, d: Data) =>
        Node.Document(t, d)
      }
    }

    def DocumentTitle: Rule1[String] = rule {
      capture(atomic {
        (CharPredicate.Alpha | '_') ~ zeroOrMore(CharPredicate.Alpha | CharPredicate.Digit | '_')
      }) ~ Whitespace
    }

    //
    // Rules that define the syntax of data nodes.
    //

    def DataNode: Rule1[Data] = rule {
      ValueNode | TableNode
    }

    def ValueNode: Rule1[Value] = rule {
      BooleanNode | NumberNode | StringNode
    }

    def BooleanNode: Rule1[Value.Boolean] = rule {
      capture(atomic("true" | "false")) ~> { (v: String) => Value.Boolean(v.toBoolean) } ~ Whitespace
    }

    def NumberNode: Rule1[Value.Number] = rule {
      capture(atomic {
        optional("+" | '-') ~ oneOrMore(CharPredicate.Digit) ~ optional(ch('.') ~ oneOrMore(CharPredicate.Digit))
      }) ~> { (v: String) => Value.Number(java.lang.Double.parseDouble(v)) } ~ Whitespace
    }

    def StringNode: Rule1[Value.String] = rule {
      SingleQuotedString | DoubleQuotedString | MultiLineString
    }

    def TableNode: Rule1[Table] = rule {
      LeftBrace ~ optional(Entries) ~ RightBrace ~> { t: Option[Seq[(Value, Data)]] =>
        Table((t getOrElse Seq.empty): _*)
      }
    }

    //
    // Rules for parsing strings.
    //

    def SingleQuotedString: Rule1[Value.String] = rule {
      "'" ~ zeroOrMore(capture(noneOf("\'\\")) | StringEscapes) ~ "'" ~> { (v: Seq[String]) =>
        Value.String(v.mkString)
      } ~ Whitespace
    }

    def DoubleQuotedString: Rule1[Value.String] = rule {
      "\"" ~ zeroOrMore(capture(noneOf("\"\\")) | StringEscapes) ~ "\"" ~> { (v: Seq[String]) =>
        Value.String(v.mkString)
      } ~ Whitespace
    }

    def MultiLineString: Rule1[Value.String] = rule {
      "[[" ~ zeroOrMore(capture(noneOf("]\\")) | StringEscapes) ~ "]]" ~> { (v: Seq[String]) =>
        Value.String(v.mkString)
      } ~ Whitespace
    }

    def StringEscapes: Rule1[String] = rule {
      capture("""\a""") ~> { _: String => BL.toString } |
        capture("""\b""") ~> { _: String => BK.toString } |
        capture("""\r""") ~> { _: String => CR.toString } |
        capture("""\n""") ~> { _: String => NL.toString } |
        capture("""\f""") ~> { _: String => FF.toString } |
        capture("""\t""") ~> { _: String => HT.toString } |
        capture("""\v""") ~> { _: String => VT.toString } |
        capture("""\\""") ~> { _: String => BS.toString } |
        capture("""\'""") ~> { _: String => SQ.toString } |
        capture("""\"""") ~> { _: String => DQ.toString } |
        capture("""\[""") ~> { _: String => LB.toString } |
        capture("""\]""") ~> { _: String => RB.toString }
    }

    //
    // Rules for parsing the content of tables.
    //

    def Key: Rule1[Value] = rule {
      LeftBracket ~ ValueNode ~ RightBracket
    }

    def Entry: Rule1[(Value, Data)] = rule {
      (Key ~ Equals ~ DataNode) ~> { (k: Value, d: Data) => k -> d }
    }

    def Entries: Rule1[Seq[(Value, Data)]] = rule {
      Entry ~ zeroOrMore(Comma ~ Entry) ~ optional(Comma) ~> { (h: (Value, Data), t: Seq[(Value, Data)]) => h +: t }
    }

    //
    // Rules for common characters.
    //

    def Comma: Rule0 = rule {
      ch(',') ~ Whitespace
    }

    def Equals: Rule0 = rule {
      ch('=') ~ Whitespace
    }

    def LeftBracket: Rule0 = rule {
      ch(LB) ~ Whitespace
    }

    def RightBracket: Rule0 = rule {
      ch(RB) ~ Whitespace
    }

    def LeftBrace: Rule0 = rule {
      ch('{') ~ Whitespace
    }

    def RightBrace: Rule0 = rule {
      ch('}') ~ Whitespace
    }

    //
    // Rules for whitespace.
    //

    def Whitespace: Rule0 = rule {
      zeroOrMore(ch(' ') | NL | CR | HT | VT | FF)
    }

  }


}
