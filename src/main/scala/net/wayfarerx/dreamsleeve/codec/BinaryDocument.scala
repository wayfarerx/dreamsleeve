package net.wayfarerx.dreamsleeve.codec

import java.io._
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import net.wayfarerx.dreamsleeve.model.{Hash, Headers, Node}

/**
 * The codec for reading and writing documents as binary blobs.
 */
object BinaryDocument {

  import Node._

  /** The header that identifies binary documents. */
  private val Header = HeaderMask | /* type = */ 0x10L | /* version = */ 0x01L

  /**
   * Reads a document from a byte array.
   *
   * @param bytes   The bytes to read from.
   * @param builder The hash builder to use for data integrity validation.
   * @return The document that was read from the byte array.
   */
  def readBytes(bytes: Array[Byte])(implicit builder: Hash.Builder): Document =
    read(new ByteArrayInputStream(bytes))

  /**
   * Reads a document from an input stream.
   *
   * @param input   The input stream to read from.
   * @param builder The hash builder to use for data integrity validation.
   * @return The document that was read from the input stream.
   */
  def read(input: InputStream)(implicit builder: Hash.Builder): Document = {
    val outer = new DataInputStream(input)
    val header = outer.readLong()
    if (header != Header) throw new MalformedDocument.InvalidHeader(header)
    val inner = new DataInputStream(new GZIPInputStream(outer))
    val bytes = new Array[Byte](32)
    inner.readFully(bytes)
    val hash = Hash(bytes)
    val title = inner.readUTF()
    val content = readData(inner.readByte(), inner) match {
      case Right(value) => value
      case Left(invalid) => throw new MalformedDocument.InvalidDataHeader(invalid)
    }
    val result = Document(title, content)
    if (result.hash != hash) throw new MalformedDocument.InvalidHash(hash, result.hash)
    result
  }

  /**
   * Reads a value node from a stream.
   *
   * @param header The header that was just previously from the stream.
   * @param input  The stream to read from.
   * @return Either the first invalid header encountered or the value node that was read.
   */
  private[codec] def readValue(header: Byte, input: DataInput): Either[Byte, Value] = header match {
    case Headers.BooleanHeader =>
      Right(Value.Boolean(input.readBoolean()))
    case Headers.NumberHeader =>
      Right(Value.Number(input.readDouble()))
    case Headers.StringHeader =>
      Right(Value.String(input.readUTF()))
    case invalid =>
      Left(invalid)
  }

  /**
   * Reads a data node from a stream.
   *
   * @param header The header that was just previously from the stream.
   * @param input  The stream to read from.
   * @return Either the first invalid header encountered or the data node that was read.
   */
  private[codec] def readData(header: Byte, input: DataInput): Either[Byte, Data] = header match {
    case Headers.TableHeader =>
      val count = input.readShort()
      ((Right(Vector[(Value, Data)]()): Either[Byte, Vector[(Value, Data)]]) /: (1 to count)) {
        case (Right(results), _) =>
          readValue(input.readByte(), input) flatMap { k =>
            readData(input.readByte(), input).map(v => results :+ (k, v))
          }
        case (left@Left(_), _) => left
      } map (Table(_: _*))
    case other =>
      readValue(other, input)
  }

  /**
   * Writes a document to an output stream.
   *
   * @param document The document to write to the output stream.
   * @param builder  The hash builder to use for data integrity validation.
   * @return The document written to a byte array.
   */
  def writeBytes(document: Document)(implicit builder: Hash.Builder): Array[Byte] = {
    val output = new ByteArrayOutputStream()
    write(document, output)
    output.toByteArray
  }

  /**
   * Writes a document to an output stream.
   *
   * @param document The document to write to the output stream.
   * @param output   The output stream to write to.
   * @param builder  The hash builder to use for data integrity validation.
   */
  def write(document: Document, output: OutputStream)(implicit builder: Hash.Builder): Unit = {
    val outer = new DataOutputStream(output) {
      // Prevent the underlying stream from being closed.
      // We must close the gzip stream for it to finish.
      override def close(): Unit = flush()
    }
    outer.writeLong(Header)
    val inner = new DataOutputStream(new GZIPOutputStream(outer))
    inner.write(document.hash.bytes)
    inner.writeUTF(document.title)
    writeData(document.content, inner)
    inner.close()
  }

  /**
   * Writes a data node to the stream.
   *
   * @param data   The data node to write.
   * @param output The stream to write to.
   *
   */
  private[codec] def writeData(data: Data, output: DataOutput): Unit = data match {
    case Value.Boolean(b) =>
      output.writeByte(Headers.BooleanHeader)
      output.writeBoolean(b)
    case Value.Number(d) =>
      output.writeByte(Headers.NumberHeader)
      output.writeDouble(d)
    case Value.String(s) =>
      output.writeByte(Headers.StringHeader)
      output.writeUTF(s)
    case Table(entries) =>
      output.writeByte(Headers.TableHeader)
      output.writeShort(entries.size)
      for ((k, v) <- entries) {
        writeData(k, output)
        writeData(v, output)
      }
  }

  /**
   * Base class for exceptions encountered while reading binary document blobs.
   *
   * @param msg The message associated with this exception.
   */
  abstract class MalformedDocument(msg: String) extends RuntimeException(msg)

  /**
   * The exceptions encountered while reading binary document blobs.
   */
  object MalformedDocument {

    /**
     * The exception thrown when an invalid document header is encountered.
     *
     * @param found The invalid document header that was found.
     */
    class InvalidHeader(found: Long) extends MalformedDocument(
      s"Invalid document header, expected ${Header.toHexString} found ${found.toHexString}.")

    /**
     * The exception thrown when an invalid data header is encountered.
     *
     * @param found The invalid data header that was found.
     */
    class InvalidDataHeader(found: Byte) extends MalformedDocument(
      s"Invalid data header: ${found.toHexString}.")

    /**
     * The exception thrown when an invalid document hash is encountered.
     *
     * @param expected The document hash that was expected.
     * @param found    The invalid document hash that was found.
     */
    class InvalidHash(expected: Hash, found: Hash) extends MalformedDocument(
      s"Invalid document hash, expected ${expected.toShortString} found ${found.toShortString}.")

  }

}
