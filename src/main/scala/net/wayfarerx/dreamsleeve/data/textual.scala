package net.wayfarerx.dreamsleeve.data

import language.implicitConversions

import java.io.{IOException, Writer}
import java.nio.{BufferOverflowException, CharBuffer, ReadOnlyBufferException}
import java.lang.{StringBuilder => JStringBuilder}

/**
 * Provides support for encoding and decoding documents to and from text.
 */
object TextualDocuments {

  import Problem.Context

  /**
   * Support for the document factory object.
   */
  trait Documents {

    /**
     * Wraps any document with the text output interface.
     *
     * @param document The document to wrap.
     * @return The text output interface.
     */
    @inline
    implicit def documentToDocumentWriter(document: Document): DocumentWriter =
    new DocumentWriter(document)

  }

  /**
   * The text output interface for documents.
   *
   * @param document The document to provide the text output interface for.
   */
  final class DocumentWriter(val document: Document) extends AnyVal {

    /**
     * Writes the document to the specified output object.
     *
     * @tparam T The type of the output object.
     * @param output The object to write to.
     * @param indent The number of times to indent new lines, defaults to zero.
     * @return Either unit or the first exception that was thrown.
     */
    def writeText[T: TextualSupport.Output](output: T, indent: Int = 0): Either[TextualProblem, Unit] = {
      val out = implicitly[TextualSupport.Output[T]]
      implicit val ctx = Context(Vector.empty)
      out.write(output, document.title) flatMap (_ => out.write(output, " = ")) flatMap { _ =>
        implicit val ctx = Context(Vector(Value.String(document.title)))
        document.content.writeText(output)
      }
    }

  }

}

/**
 * Provides support for encoding and decoding fragments to and from text.
 */
object TextualFragments {

  import Problem.Context
  import TextualProblem.Attempt

  /**
   * Support for the fragment factory object.
   */
  trait Fragments {

    /**
     * Wraps any fragment with the text output interface.
     *
     * @param fragment The fragment to wrap.
     * @return The text output interface.
     */
    @inline
    implicit def fragmentToFragmentWriter(fragment: Fragment): FragmentWriter =
    new FragmentWriter(fragment)

  }

  /**
   * The text output interface for fragments.
   *
   * @param fragment The fragment to provide the text output interface for.
   */
  final class FragmentWriter(val fragment: Fragment) extends AnyVal {

    /**
     * Writes the fragment to the specified output object.
     *
     * @tparam T The type of the output object.
     * @param output The object to write to.
     * @param indent The number of times to indent new lines, defaults to zero.
     * @param ctx    The context of the fragment writing operation.
     * @return Either unit or the first exception that was thrown.
     */
    def writeText[T: TextualSupport.Output](output: T, indent: Int = 0)(implicit ctx: Context): Attempt[Unit] =
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

  import Problem.Context
  import TextualProblem.Attempt

  /** The 16-bit bell character. */
  val Bell: Char = 0x00000007.toChar

  /** The 16-bit vertical tab character. */
  val VerticalTab: Char = 0x0000000B.toChar

  /**
   * Support for the value factory object.
   */
  trait Values {

    /**
     * Wraps any value with the text output interface.
     *
     * @param value The value to wrap.
     * @return The text output interface.
     */
    @inline
    implicit def valueToValueWriter(value: Value): ValueWriter =
    new ValueWriter(value)

  }

  /**
   * Support for the boolean factory object.
   */
  trait Booleans {

    /**
     * Wraps any boolean with the text output interface.
     *
     * @param boolean The boolean to wrap.
     * @return The text output interface.
     */
    @inline
    implicit def booleanToBooleanWriter(boolean: Value.Boolean): BooleanWriter =
    new BooleanWriter(boolean)

  }

  /**
   * Support for the number factory object.
   */
  trait Numbers {

    /**
     * Wraps any number with the text output interface.
     *
     * @param number The number to wrap.
     * @return The text output interface.
     */
    @inline
    implicit def numberToNumberWriter(number: Value.Number): NumberWriter =
    new NumberWriter(number)

  }

  /**
   * Support for the string factory object.
   */
  trait Strings {

    /**
     * Wraps any string with the text output interface.
     *
     * @param string The string to wrap.
     * @return The text output interface.
     */
    @inline
    implicit def numberToNumberWriter(string: Value.String): StringWriter =
    new StringWriter(string)

  }

  /**
   * The text output interface for values.
   *
   * @param value The value to provide the text output interface for.
   */
  final class ValueWriter(val value: Value) extends AnyVal {

    /**
     * Writes the value to the specified output object.
     *
     * @tparam T The type of the output object.
     * @param output The object to write to.
     * @param ctx    The context of the value writing operation.
     * @return Either unit or the first exception that was thrown.
     */
    def writeText[T: TextualSupport.Output](output: T)(implicit ctx: Context): Attempt[Unit] =
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

    /**
     * Writes the value to the specified output object.
     *
     * @tparam T The type of the output object.
     * @param output The object to write to.
     * @param ctx    The context of the boolean writing operation.
     * @return Either unit or the first exception that was thrown.
     */
    def writeText[T: TextualSupport.Output](output: T)(implicit ctx: Context): Attempt[Unit] = {
      implicitly[TextualSupport.Output[T]].write(output, boolean.value.toString)
    }

  }

  /**
   * The text output interface for number values.
   *
   * @param number The number to provide the text output interface for.
   */
  final class NumberWriter(val number: Value.Number) extends AnyVal {

    /**
     * Writes the value to the specified output object.
     *
     * @tparam T The type of the output object.
     * @param output The object to write to.
     * @param ctx    The context of the number writing operation.
     * @return Either unit or the first exception that was thrown.
     */
    def writeText[T: TextualSupport.Output](output: T)(implicit ctx: Context): Attempt[Unit] = {
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

    /**
     * Writes the value to the specified output object.
     *
     * @tparam T The type of the output object.
     * @param output The object to write to.
     * @param ctx    The context of the string writing operation.
     * @return Either unit or the first exception that was thrown.
     */
    def writeText[T: TextualSupport.Output](output: T)(implicit ctx: Context): Attempt[Unit] = {
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

  import Problem.Context
  import TextualProblem.Attempt

  /** The value that is used to indent nested lines. */
  val Indent = "    "

  /**
   * Support for the table factory object.
   */
  trait Tables {

    /**
     * Wraps any table with the text output interface.
     *
     * @param table The table to wrap.
     * @return The text output interface.
     */
    @inline
    implicit def tableToTableWriter(table: Table): TableWriter =
    new TableWriter(table)

  }

  /**
   * The text output interface for tables.
   *
   * @param table The table to provide the text output interface for.
   */
  final class TableWriter(val table: Table) extends AnyVal {

    /**
     * Writes the table to the specified output object.
     *
     * @tparam T The type of the output object.
     * @param output The object to write to.
     * @param indent The number of times to indent new lines, defaults to zero.
     * @param ctx    The context of the table writing operation.
     * @return Either unit or the first exception that was thrown.
     */
    def writeText[T: TextualSupport.Output](output: T, indent: Int = 0)(implicit ctx: Context): Attempt[Unit] = {
      val out = implicitly[TextualSupport.Output[T]]
      val deeper = indent + 1
      val short = Indent * indent
      val long = Indent * deeper
      val _ctx = ctx
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
          implicit val ctx = _ctx.push(k)
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

  import Problem.Context
  import TextualProblem.Attempt

  /** The line break that is written. */
  val LineBreak = "\r\n"

  /**
   * The type class for objects that can be written to.
   *
   * @tparam T The type of object that can  be written to.
   */
  trait Output[T] {

    /**
     * Writes a character to the target object.
     *
     * @param target The object to write to.
     * @param c      The character to write.
     * @param ctx    The context of the write operation.
     * @return Either unit or the first exception that was thrown.
     */
    def write(target: T, c: Char)(implicit ctx: Context): Attempt[Unit]

    /**
     * Writes a string of characters to the target object.
     *
     * @param target The object to write to.
     * @param string The string of characters to write.
     * @param ctx    The context of the write operation.
     * @return Either unit or the first exception that was thrown.
     */
    def write(target: T, string: String)(implicit ctx: Context): Attempt[Unit]

    /**
     * Writes a line break to the target object.
     *
     * @param target The object to write to.
     * @param ctx    The context of the write operation.
     * @return Either unit or the first exception that was thrown.
     */
    @inline
    final def writeln(target: T)(implicit ctx: Context): Attempt[Unit] =
    write(target, LineBreak)

  }

  /**
   * Supported output implementations.
   */
  object Output {

    /** The implicit output implementation for string builders. */
    implicit val StringBuilderOutput: Output[StringBuilder] = new Output[StringBuilder] {

      override def write(target: StringBuilder, c: Char)(implicit ctx: Context): Attempt[Unit] =
        Right(target.append(c))

      override def write(target: StringBuilder, string: String)(implicit ctx: Context): Attempt[Unit] =
        Right(target.append(string))

    }

    /** The implicit output implementation for Java string builders. */
    implicit val JStringBuilderOutput: Output[JStringBuilder] = new Output[JStringBuilder] {

      override def write(target: JStringBuilder, c: Char)(implicit ctx: Context): Attempt[Unit] =
        Right(target.append(c))

      override def write(target: JStringBuilder, string: String)(implicit ctx: Context): Attempt[Unit] =
        Right(target.append(string))

    }

    /** The implicit output implementation for string buffers. */
    implicit val StringBufferOutput: Output[StringBuffer] = new Output[StringBuffer] {

      override def write(target: StringBuffer, c: Char)(implicit ctx: Context): Attempt[Unit] =
        Right(target.append(c))

      override def write(target: StringBuffer, string: String)(implicit ctx: Context): Attempt[Unit] =
        Right(target.append(string))

    }

    /** The implicit output implementation for character buffers. */
    implicit val CharBufferOutput: Output[CharBuffer] = new Output[CharBuffer] {

      override def write(target: CharBuffer, c: Char)(implicit ctx: Context): Attempt[Unit] =
        try Right(target.append(c)) catch {
          case e: BufferOverflowException => Left(TextualProblem.Overflow(e))
          case e: ReadOnlyBufferException => Left(TextualProblem.ReadOnly(e))
        }

      override def write(target: CharBuffer, string: String)(implicit ctx: Context): Attempt[Unit] =
        try Right(target.append(string)) catch {
          case e: BufferOverflowException => Left(TextualProblem.Overflow(e))
          case e: ReadOnlyBufferException => Left(TextualProblem.ReadOnly(e))
        }

    }

    /** The implicit output implementation for writers. */
    implicit val WriterOutput: Output[Writer] = new Output[Writer] {

      override def write(target: Writer, c: Char)(implicit ctx: Context): Attempt[Unit] =
        try Right(target.write(c)) catch {
          case e: IOException => Left(TextualProblem.IO(e))
        }

      override def write(target: Writer, string: String)(implicit ctx: Context): Attempt[Unit] =
        try Right(target.write(string)) catch {
          case e: IOException => Left(TextualProblem.IO(e))
        }

    }

  }

}

/**
 * Base class for problems that occur while reading or writing text.
 */
sealed trait TextualProblem extends Problem

/**
 * Concrete problem implementations.
 */
object TextualProblem extends Problem.Factory[TextualProblem] {

  /** The type that represents the result of error-prone text operations. */
  final type Attempt[T] = Either[TextualProblem, T]

  /**
   * Problem returned when an IO exception is encountered.
   *
   * @param exception The exception that was encountered.
   * @param context   The context where the problem occurred.
   */
  case class IO(exception: IOException)(implicit val context: Context) extends TextualProblem

  /**
   * Problem returned when a buffer is overflowed while writing.
   *
   * @param exception The exception that was encountered.
   * @param context   The context where the problem occurred.
   */
  case class Overflow(exception: BufferOverflowException)(implicit val context: Context) extends TextualProblem

  /**
   * Problem returned when attempting to write to a read only buffer.
   *
   * @param exception The exception that was encountered.
   * @param context   The context where the problem occurred.
   */
  case class ReadOnly(exception: ReadOnlyBufferException)(implicit val context: Context) extends TextualProblem

}
