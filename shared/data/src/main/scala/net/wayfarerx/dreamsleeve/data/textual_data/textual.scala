package net.wayfarerx.dreamsleeve.data.textual_data

import java.io.{IOException, Reader, Writer}
import java.lang.{StringBuilder => JStringBuilder}
import java.nio.{BufferOverflowException, CharBuffer, ReadOnlyBufferException}

import net.wayfarerx.dreamsleeve.data._
import org.parboiled2._

import scala.language.implicitConversions
import scala.util.{Failure, Success}


/**
 * Provides support for encoding and decoding documents to and from text.
 */
object TextualDocuments {

  /**
   * Support for the document factory object.
   */
  trait Documents {

    import TextualProblem.Reading.Attempt

    /**
     * Wraps any document with the text output interface.
     *
     * @param document The document to wrap.
     * @return The text output interface.
     */
    @inline
    implicit def documentToDocumentWriter(document: Document): DocumentWriter =
    new DocumentWriter(document)

    /**
     * Reads a document from the specified input object.
     *
     * @tparam T The type of the input object.
     * @param input The object to read from.
     * @return Either the document that was read or the first exception that was thrown.
     */
    final def readText[T: TextualSupport.Input](input: T): Attempt[Document] = {
      implicitly[TextualSupport.Input[T]].toParserInput(input) flatMap { in =>
        new TextualSupport.TextParser(in).Documents.run() match {
          case Success(document) => Right(document)
          case Failure(thrown) => Left(TextualProblem.Syntax(thrown))
        }
      }
    }

  }

  /**
   * The text output interface for documents.
   *
   * @param document The document to provide the text output interface for.
   */
  final class DocumentWriter(val document: Document) extends AnyVal {

    import TextualProblem.Writing.Attempt

    /**
     * Writes the document to the specified output object.
     *
     * @tparam T The type of the output object.
     * @param output The object to write to.
     * @param indent The number of times to indent new lines, defaults to zero.
     * @return Either unit or the first exception that was thrown.
     */
    def writeText[T: TextualSupport.Output](output: T, indent: Int = 0): Attempt[Unit] = {
      val out = implicitly[TextualSupport.Output[T]]
      out.write(output, document.title) flatMap (_ => out.write(output, " = ")) flatMap { _ =>
        document.content.writeText(output, indent)
      }
    }

  }

}

/**
 * Provides support for encoding and decoding fragments to and from text.
 */
object TextualFragments {

  /**
   * Support for the fragment factory object.
   */
  trait Fragments {

    import TextualProblem.Reading.Attempt

    /**
     * Wraps any fragment with the text output interface.
     *
     * @param fragment The fragment to wrap.
     * @return The text output interface.
     */
    @inline
    implicit def fragmentToFragmentWriter(fragment: Fragment): FragmentWriter =
    new FragmentWriter(fragment)

    /**
     * Reads a fragment from the specified input object.
     *
     * @tparam T The type of the input object.
     * @param input The object to read from.
     * @return Either the fragment that was read or the first exception that was thrown.
     */
    final def readText[T: TextualSupport.Input](input: T): Attempt[Fragment] = {
      implicitly[TextualSupport.Input[T]].toParserInput(input) flatMap { in =>
        new TextualSupport.TextParser(in).Fragments.run() match {
          case Success(fragment) => Right(fragment)
          case Failure(thrown) => Left(TextualProblem.Syntax(thrown))
        }
      }
    }

  }

  /**
   * The text output interface for fragments.
   *
   * @param fragment The fragment to provide the text output interface for.
   */
  final class FragmentWriter(val fragment: Fragment) extends AnyVal {

    import TextualProblem.Writing.Attempt

    /**
     * Writes the fragment to the specified output object.
     *
     * @tparam T The type of the output object.
     * @param output The object to write to.
     * @param indent The number of times to indent new lines, defaults to zero.
     * @return Either unit or the first exception that was thrown.
     */
    def writeText[T: TextualSupport.Output](output: T, indent: Int = 0): Attempt[Unit] =
      fragment match {
        case v@Value() => v.writeText(output)
        case t@Table(_) => t.writeText(output, indent)
      }

  }

}

/**
 * Provides support for encoding and decoding values to and from text.
 */
object TextualValues {

  /** The 16-bit bell character. */
  val Bell: Char = 0x00000007.toChar

  /** The 16-bit vertical tab character. */
  val VerticalTab: Char = 0x0000000B.toChar

  /**
   * Support for the value factory object.
   */
  trait Values {

    import TextualProblem.Reading.Attempt

    /**
     * Wraps any value with the text output interface.
     *
     * @param value The value to wrap.
     * @return The text output interface.
     */
    @inline
    implicit def valueToValueWriter(value: Value): ValueWriter =
    new ValueWriter(value)

    /**
     * Reads a value from the specified input object.
     *
     * @tparam T The type of the input object.
     * @param input The object to read from.
     * @return Either the value that was read or the first exception that was thrown.
     */
    final def readText[T: TextualSupport.Input](input: T): Attempt[Value] = {
      implicitly[TextualSupport.Input[T]].toParserInput(input) flatMap { in =>
        new TextualSupport.TextParser(in).Values.run() match {
          case Success(value) => Right(value)
          case Failure(thrown) => Left(TextualProblem.Syntax(thrown))
        }
      }
    }

  }

  /**
   * Support for the boolean factory object.
   */
  trait Booleans {

    import TextualProblem.Reading.Attempt

    /**
     * Wraps any boolean with the text output interface.
     *
     * @param boolean The boolean to wrap.
     * @return The text output interface.
     */
    @inline
    implicit def booleanToBooleanWriter(boolean: Value.Boolean): BooleanWriter =
    new BooleanWriter(boolean)

    /**
     * Reads a boolean from the specified input object.
     *
     * @tparam T The type of the input object.
     * @param input The object to read from.
     * @return Either the boolean that was read or the first exception that was thrown.
     */
    final def readText[T: TextualSupport.Input](input: T): Attempt[Value.Boolean] = {
      implicitly[TextualSupport.Input[T]].toParserInput(input) flatMap { in =>
        new TextualSupport.TextParser(in).Booleans.run() match {
          case Success(boolean) => Right(boolean)
          case Failure(thrown) => Left(TextualProblem.Syntax(thrown))
        }
      }
    }

  }

  /**
   * Support for the number factory object.
   */
  trait Numbers {

    import TextualProblem.Reading.Attempt

    /**
     * Wraps any number with the text output interface.
     *
     * @param number The number to wrap.
     * @return The text output interface.
     */
    @inline
    implicit def numberToNumberWriter(number: Value.Number): NumberWriter =
    new NumberWriter(number)

    /**
     * Reads a number from the specified input object.
     *
     * @tparam T The type of the input object.
     * @param input The object to read from.
     * @return Either the number that was read or the first exception that was thrown.
     */
    final def readText[T: TextualSupport.Input](input: T): Attempt[Value.Number] = {
      implicitly[TextualSupport.Input[T]].toParserInput(input) flatMap { in =>
        new TextualSupport.TextParser(in).Numbers.run() match {
          case Success(number) => Right(number)
          case Failure(thrown) => Left(TextualProblem.Syntax(thrown))
        }
      }
    }

  }

  /**
   * Support for the string factory object.
   */
  trait Strings {

    import TextualProblem.Reading.Attempt

    /**
     * Wraps any string with the text output interface.
     *
     * @param string The string to wrap.
     * @return The text output interface.
     */
    @inline
    implicit def numberToNumberWriter(string: Value.String): StringWriter =
    new StringWriter(string)

    /**
     * Reads a string from the specified input object.
     *
     * @tparam T The type of the input object.
     * @param input The object to read from.
     * @return Either the string that was read or the first exception that was thrown.
     */
    final def readText[T: TextualSupport.Input](input: T): Attempt[Value.String] = {
      implicitly[TextualSupport.Input[T]].toParserInput(input) flatMap { in =>
        new TextualSupport.TextParser(in).Strings.run() match {
          case Success(string) => Right(string)
          case Failure(thrown) => Left(TextualProblem.Syntax(thrown))
        }
      }
    }

  }

  /**
   * The text output interface for values.
   *
   * @param value The value to provide the text output interface for.
   */
  final class ValueWriter(val value: Value) extends AnyVal {

    import TextualProblem.Writing.Attempt

    /**
     * Writes the value to the specified output object.
     *
     * @tparam T The type of the output object.
     * @param output The object to write to.
     * @return Either unit or the first exception that was thrown.
     */
    def writeText[T: TextualSupport.Output](output: T): Attempt[Unit] =
      value match {
        case b@Value.Boolean(_) => b.writeText(output)
        case n@Value.Number(_) => n.writeText(output)
        case s@Value.String(_) => s.writeText(output)
      }

  }

  /**
   * The text output interface for boolean values.
   *
   * @param boolean The boolean to provide the text output interface for.
   */
  final class BooleanWriter(val boolean: Value.Boolean) extends AnyVal {

    import TextualProblem.Writing.Attempt

    /**
     * Writes the value to the specified output object.
     *
     * @tparam T The type of the output object.
     * @param output The object to write to.
     * @return Either unit or the first exception that was thrown.
     */
    def writeText[T: TextualSupport.Output](output: T): Attempt[Unit] = {
      implicitly[TextualSupport.Output[T]].write(output, boolean.value.toString)
    }

  }

  /**
   * The text output interface for number values.
   *
   * @param number The number to provide the text output interface for.
   */
  final class NumberWriter(val number: Value.Number) extends AnyVal {

    import TextualProblem.Writing.Attempt

    /**
     * Writes the value to the specified output object.
     *
     * @tparam T The type of the output object.
     * @param output The object to write to.
     * @return Either unit or the first exception that was thrown.
     */
    def writeText[T: TextualSupport.Output](output: T): Attempt[Unit] = {
      val out = implicitly[TextualSupport.Output[T]]
      number.value match {
        case value if value.isNaN => out.write(output, '0')
        case value if value.isNegInfinity => out.write(output, Double.MinValue.toString)
        case value if value.isPosInfinity => out.write(output, Double.MaxValue.toString)
        case value if value == Math.floor(value) => out.write(output, value.toLong.toString)
        case value => out.write(output, value.toString)
      }
    }

  }

  /**
   * The text output interface for string values.
   *
   * @param string The string to provide the text output interface for.
   */
  final class StringWriter(val string: Value.String) extends AnyVal {

    import TextualProblem.Writing.Attempt

    /**
     * Writes the value to the specified output object.
     *
     * @tparam T The type of the output object.
     * @param output The object to write to.
     * @return Either unit or the first exception that was thrown.
     */
    def writeText[T: TextualSupport.Output](output: T): Attempt[Unit] = {
      val out = implicitly[TextualSupport.Output[T]]
      (out.write(output, '"') /: string.value) { (previous, c) =>
        previous flatMap (_ => c match {
          case 0 => out.write(output, """\0""")
          case Bell => out.write(output, """\a""")
          case '\b' => out.write(output, """\b""")
          case '\f' => out.write(output, """\f""")
          case '\n' => out.write(output, """\n""")
          case '\r' => out.write(output, """\r""")
          case '\t' => out.write(output, """\t""")
          case VerticalTab => out.write(output, """\v""")
          case '\\' => out.write(output, """\\""")
          case '"' => out.write(output, """\"""")
          case _ => out.write(output, c)
        })
      } flatMap (_ => out.write(output, '"'))
    }

  }

}

/**
 * Provides support for encoding and decoding tables to and from text.
 */
object TextualTables {

  /** The value that is used to indent nested lines. */
  val Indent: String = " " * 4

  /**
   * Support for the table factory object.
   */
  trait Tables {

    import TextualProblem.Reading.Attempt

    /**
     * Wraps any table with the text output interface.
     *
     * @param table The table to wrap.
     * @return The text output interface.
     */
    @inline
    implicit def tableToTableWriter(table: Table): TableWriter =
    new TableWriter(table)

    /**
     * Reads a table from the specified input object.
     *
     * @tparam T The type of the input object.
     * @param input The object to read from.
     * @return Either the table that was read or the first exception that was thrown.
     */
    final def readText[T: TextualSupport.Input](input: T): Attempt[Table] = {
      implicitly[TextualSupport.Input[T]].toParserInput(input) flatMap { in =>
        new TextualSupport.TextParser(in).Tables.run() match {
          case Success(table) => Right(table)
          case Failure(thrown) => Left(TextualProblem.Syntax(thrown))
        }
      }
    }

  }

  /**
   * The text output interface for tables.
   *
   * @param table The table to provide the text output interface for.
   */
  final class TableWriter(val table: Table) extends AnyVal {

    import TextualProblem.Writing.Attempt

    /**
     * Writes the table to the specified output object.
     *
     * @tparam T The type of the output object.
     * @param output The object to write to.
     * @param indent The number of times to indent new lines, defaults to zero.
     * @return Either unit or the first exception that was thrown.
     */
    def writeText[T: TextualSupport.Output](output: T, indent: Int = 0): Attempt[Unit] = {
      val out = implicitly[TextualSupport.Output[T]]
      val deeper = indent + 1
      val short = Indent * indent
      val long = Indent * deeper
      ((out.writeln(output) flatMap
        (_ => out.write(output, short)) flatMap
        (_ => out.write(output, '{')) flatMap
        (_ => out.writeln(output))) /: table.entries) { (previous, entry) =>
        val (k, v) = entry
        previous flatMap
          (_ => out.write(output, long)) flatMap
          (_ => out.write(output, '[')) flatMap
          (_ => k.writeText(output)) flatMap
          (_ => out.write(output, ']')) flatMap
          (_ => out.write(output, " = ")) flatMap { _ =>
          v.writeText(output, deeper)
        } flatMap
          (_ => out.write(output, ',')) flatMap
          (_ => out.writeln(output))
      } flatMap
        (_ => out.write(output, short)) flatMap
        (_ => out.write(output, '}'))
    }

  }


}

/**
 * Definition of the text input/output interface.
 */
object TextualSupport {

  /** The bell character. */
  val Bell: String = 0x00000007.toChar.toString
  /** The line break that is written. */
  val LineBreak = "\r\n"
  /** The vertical tab character. */
  val VerticalTab: Char = 0x0000000B.toChar

  /**
   * The type class for objects that can be read from.
   *
   * @tparam T The type of object that can be read from.
   */
  trait Input[T] {

    import TextualProblem.Reading.Attempt

    /**
     * Creates a parser input object from the target object.
     *
     * @param target The object to create a parser input from.
     * @return The parser input created from the target object.
     */
    def toParserInput(target: T): Attempt[ParserInput]

  }

  /**
   * Supported input implementations.
   */
  object Input {

    /** The implicit input implementation for character builders. */
    implicit val CharSequenceInput: Input[CharSequence] = { (target: CharSequence) =>
      Right(new ParserInput.DefaultParserInput {

        override def length: Int =
          target.length

        override def charAt(ix: Int): Char =
          target.charAt(ix)

        override def sliceString(start: Int, end: Int): String =
          target.subSequence(start, end).toString

        override def sliceCharArray(start: Int, end: Int): Array[Char] = {
          val chars = new Array[Char](end - start)
          for (i <- chars.indices) chars(i) = target.charAt(start + i)
          chars
        }

      })
    }

    /** The implicit input implementation for readers. */
    implicit val ReaderInput: Input[Reader] = { (target: Reader) =>
      val buffer = new Array[Char](128)
      val result = new StringBuilder(buffer.length)
      try {
        var read = target.read(buffer)
        while (read >= 0) {
          if (read > 0) result.append(buffer, 0, read)
          read = target.read(buffer)
        }
        CharSequenceInput.toParserInput(result)
      } catch {
        case e: IOException => Left(TextualProblem.IO(e))
      }
    }

  }

  /**
   * The type class for objects that can be written to.
   *
   * @tparam T The type of object that can be written to.
   */
  trait Output[T] {

    import TextualProblem.Writing.Attempt

    /**
     * Writes a character to the target object.
     *
     * @param target The object to write to.
     * @param c      The character to write.
     * @return Either unit or the first exception that was thrown.
     */
    def write(target: T, c: Char): Attempt[Unit]

    /**
     * Writes a string of characters to the target object.
     *
     * @param target The object to write to.
     * @param string The string of characters to write.
     * @return Either unit or the first exception that was thrown.
     */
    def write(target: T, string: String): Attempt[Unit]

    /**
     * Writes a line break to the target object.
     *
     * @param target The object to write to.
     * @return Either unit or the first exception that was thrown.
     */
    @inline
    final def writeln(target: T): Attempt[Unit] =
    write(target, LineBreak)

  }

  /**
   * Supported output implementations.
   */
  object Output {

    import TextualProblem.Writing.Attempt

    /** The implicit output implementation for string builders. */
    implicit val StringBuilderOutput: Output[StringBuilder] = new Output[StringBuilder] {

      override def write(target: StringBuilder, c: Char): Attempt[Unit] =
        Right(target.append(c))

      override def write(target: StringBuilder, string: String): Attempt[Unit] =
        Right(target.append(string))

    }

    /** The implicit output implementation for Java string builders. */
    implicit val JStringBuilderOutput: Output[JStringBuilder] = new Output[JStringBuilder] {

      override def write(target: JStringBuilder, c: Char): Attempt[Unit] =
        Right(target.append(c))

      override def write(target: JStringBuilder, string: String): Attempt[Unit] =
        Right(target.append(string))

    }

    /** The implicit output implementation for string buffers. */
    implicit val StringBufferOutput: Output[StringBuffer] = new Output[StringBuffer] {

      override def write(target: StringBuffer, c: Char): Attempt[Unit] =
        Right(target.append(c))

      override def write(target: StringBuffer, string: String): Attempt[Unit] =
        Right(target.append(string))

    }

    /** The implicit output implementation for character buffers. */
    implicit val CharBufferOutput: Output[CharBuffer] = new Output[CharBuffer] {

      override def write(target: CharBuffer, c: Char): Attempt[Unit] =
        try Right(target.append(c)) catch {
          case e: BufferOverflowException => Left(TextualProblem.Overflow(e))
          case e: ReadOnlyBufferException => Left(TextualProblem.ReadOnly(e))
        }

      override def write(target: CharBuffer, string: String): Attempt[Unit] =
        try Right(target.append(string)) catch {
          case e: BufferOverflowException => Left(TextualProblem.Overflow(e))
          case e: ReadOnlyBufferException => Left(TextualProblem.ReadOnly(e))
        }

    }

    /** The implicit output implementation for writers. */
    implicit val WriterOutput: Output[Writer] = new Output[Writer] {

      override def write(target: Writer, c: Char): Attempt[Unit] =
        try Right(target.write(c)) catch {
          case e: IOException => Left(TextualProblem.IO(e))
        }

      override def write(target: Writer, string: String): Attempt[Unit] =
        try Right(target.write(string)) catch {
          case e: IOException => Left(TextualProblem.IO(e))
        }

    }

  }

  /**
   * A parser for documents & fragments written as text.
   *
   * @param input The input to this parser.
   */
  private[data] final class TextParser(override val input: ParserInput) extends Parser {

    /** The type of key/value pairs. */
    private type Entry = (Value, Fragment)

    //
    // External rules for documents and fragments.
    //

    /** The top-level parser for documents. */
    def Documents: Rule1[Document] = rule {
      Whitespace ~ DocumentParser ~ EOI
    }

    /** The top-level parser for fragments. */
    def Fragments: Rule1[Fragment] = rule {
      Whitespace ~ FragmentParser ~ EOI
    }

    /** The top-level parser for values. */
    def Values: Rule1[Value] = rule {
      Whitespace ~ ValueParser ~ EOI
    }

    /** The top-level parser for booleans. */
    def Booleans: Rule1[Value.Boolean] = rule {
      Whitespace ~ BooleanParser ~ EOI
    }

    /** The top-level parser for numbers. */
    def Numbers: Rule1[Value.Number] = rule {
      Whitespace ~ NumberParser ~ EOI
    }

    /** The top-level parser for strings. */
    def Strings: Rule1[Value.String] = rule {
      Whitespace ~ StringParser ~ EOI
    }

    /** The top-level parser for tables. */
    def Tables: Rule1[Table] = rule {
      Whitespace ~ TableParser ~ EOI
    }

    //
    // Internal rules for fragments and other syntax components.
    //

    /** The internal parser for documents. */
    private def DocumentParser: Rule1[Document] = rule {
      TitleParser ~ Equals ~ FragmentParser ~> Document.apply _
    }

    /** The internal parser for titles. */
    private def TitleParser: Rule1[String] = rule {
      capture(atomic((CharPredicate.Alpha | '_') ~ zeroOrMore(CharPredicate.Alpha | CharPredicate.Digit | '_'))) ~
        Whitespace
    }

    /** The internal parser for fragments. */
    private def FragmentParser: Rule1[Fragment] = rule {
      ValueParser | TableParser
    }

    /** The internal parser for values. */
    private def ValueParser: Rule1[Value] = rule {
      BooleanParser | NumberParser | StringParser
    }

    /** The internal parser for booleans. */
    private def BooleanParser: Rule1[Value.Boolean] = rule {
      capture(atomic("true" | "false")) ~> { (v: String) => Value.Boolean(v.toBoolean) } ~ Whitespace
    }

    /** The internal parser for numbers. */
    private def NumberParser: Rule1[Value.Number] = rule {
      capture(atomic {
        optional("+" | '-') ~ oneOrMore(CharPredicate.Digit) ~ optional(ch('.') ~ oneOrMore(CharPredicate.Digit))
      }) ~> { (v: String) => Value.Number(java.lang.Double.parseDouble(v)) } ~ Whitespace
    }

    /** The internal parser for strings. */
    private def StringParser: Rule1[Value.String] = rule {
      SingleQuotedString | DoubleQuotedString | BracketedString
    }

    /** The internal parser for tables. */
    private def TableParser: Rule1[Table] = rule {
      LeftBrace ~ optional(Entries) ~ RightBrace ~> { t: Option[Seq[Entry]] => Table(t getOrElse Seq.empty: _*) }
    }

    /** The internal parser for single-quoted strings. */
    private def SingleQuotedString: Rule1[Value.String] = rule {
      "'" ~ zeroOrMore(capture(noneOf("\'\\")) | StringEscape) ~ "'" ~> { (v: Seq[String]) =>
        Value.String(v.mkString)
      } ~ Whitespace
    }

    /** The internal parser for double-quoted strings. */
    private def DoubleQuotedString: Rule1[Value.String] = rule {
      '"' ~ zeroOrMore(capture(noneOf("\"\\")) | StringEscape) ~ '"' ~> { (v: Seq[String]) =>
        Value.String(v.mkString)
      } ~ Whitespace
    }

    /** The internal parser for bracketed strings. */
    private def BracketedString: Rule1[Value.String] = rule {
      "[[" ~ zeroOrMore(capture(noneOf("]\\")) | StringEscape) ~ "]]" ~> { (v: Seq[String]) =>
        Value.String(v.mkString)
      } ~ Whitespace
    }

    /** The internal parser for string escapes. */
    private def StringEscape: Rule1[String] = rule {
      capture("""\a""") ~> { _: String => Bell } |
        capture("""\b""") ~> { _: String => "\b" } |
        capture("""\r""") ~> { _: String => "\r" } |
        capture("""\n""") ~> { _: String => "\n" } |
        capture("""\f""") ~> { _: String => "\f" } |
        capture("""\t""") ~> { _: String => "\t" } |
        capture("""\v""") ~> { _: String => VerticalTab.toString } |
        capture("""\\""") ~> { _: String => "\\" } |
        capture("""\'""") ~> { _: String => "'" } |
        capture("""\"""") ~> { _: String => "\"" } |
        capture("""\[""") ~> { _: String => "[" } |
        capture("""\]""") ~> { _: String => "]" }
    }

    /** The internal parser for table keys. */
    private def Key: Rule1[Value] = rule {
      LeftBracket ~ ValueParser ~ RightBracket
    }

    /** The internal parser for table entries. */
    private def Entry: Rule1[Entry] = rule {
      (Key ~ Equals ~ FragmentParser) ~> { (k: Value, d: Fragment) => k -> d }
    }

    /** The internal parser for table entry sequences. */
    private def Entries: Rule1[Seq[Entry]] = rule {
      Entry ~ zeroOrMore(Comma ~ Entry) ~ optional(Comma) ~> { (h: Entry, t: Seq[Entry]) => h +: t }
    }

    /** The internal parser for commas. */
    private def Comma: Rule0 = rule {
      ch(',') ~ Whitespace
    }

    /** The internal parser for equals signs. */
    private def Equals: Rule0 = rule {
      ch('=') ~ Whitespace
    }

    /** The internal parser for left brackets. */
    private def LeftBracket: Rule0 = rule {
      ch('[') ~ Whitespace
    }

    /** The internal parser for right brackets. */
    private def RightBracket: Rule0 = rule {
      ch(']') ~ Whitespace
    }

    /** The internal parser for left braces. */
    private def LeftBrace: Rule0 = rule {
      ch('{') ~ Whitespace
    }

    /** The internal parser for right braces. */
    private def RightBrace: Rule0 = rule {
      ch('}') ~ Whitespace
    }

    /** The internal parser for optional whitespace. */
    private def Whitespace: Rule0 = rule {
      zeroOrMore(ch(' ') | '\n' | '\r' | '\t' | VerticalTab | '\f')
    }

  }

}

/**
 * Base class for problems that occur while reading or writing text.
 */
sealed trait TextualProblem extends DataProblem

/**
 * Concrete problem implementations.
 */
object TextualProblem extends DataProblem {

  /**
   * Marker trait for problems that can occur during input operations.
   */
  trait Reading extends DataProblem

  /**
   * Definitions associated with input problems.
   */
  object Reading {

    /** The type that represents the result of error-prone text input operations. */
    final type Attempt[T] = Either[Reading, T]

  }

  /**
   * Marker trait for problems that can occur during output operations.
   */
  trait Writing extends DataProblem

  /**
   * Definitions associated with output problems.
   */
  object Writing {

    /** The type that represents the result of error-prone text output operations. */
    final type Attempt[T] = Either[Writing, T]

  }

  /**
   * Problem returned when a syntax error is encountered while reading.
   *
   * @param exception The exception that was encountered.
   */
  case class Syntax(exception: Throwable) extends Reading

  /**
   * Problem returned when a buffer is overflowed while writing.
   *
   * @param exception The exception that was encountered.
   */
  case class Overflow(exception: BufferOverflowException) extends Writing

  /**
   * Problem returned when attempting to write to a read only buffer.
   *
   * @param exception The exception that was encountered.
   */
  case class ReadOnly(exception: ReadOnlyBufferException) extends Writing

  /**
   * Problem returned when an IO exception is encountered.
   *
   * @param exception The exception that was encountered.
   */
  case class IO(exception: IOException) extends Reading with Writing

}
