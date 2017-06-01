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

    /* Reads a value node from the stream. */
    def readValue(header: Byte): Value = header match {
      case Headers.BooleanHeader =>
        Value.Boolean(inner.readBoolean())
      case Headers.NumberHeader =>
        Value.Number(inner.readDouble())
      case Headers.StringHeader =>
        Value.String(inner.readUTF())
      case invalid =>
        throw new MalformedDocument.InvalidDataHeader(invalid)
    }

    /* Reads a data node from the stream. */
    def readData(): Data = inner.readByte() match {
      case Headers.TableHeader =>
        val count = inner.readShort()
        Table((1 to count) map (_ => readValue(inner.readByte()) -> readData()): _*)
      case other =>
        readValue(other)
    }

    val bytes = new Array[Byte](32)
    inner.readFully(bytes)
    val hash = Hash(bytes)
    val title = inner.readUTF()
    val content = readData()
    val result = Document(title, content)
    if (result.hash != hash) throw new MalformedDocument.InvalidHash(hash, result.hash)
    result
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

    /* Writes a value node to the stream. */
    def writeData(data: Data): Unit = data match {
      case Value.Boolean(b) =>
        inner.writeByte(Headers.BooleanHeader)
        inner.writeBoolean(b)
      case Value.Number(d) =>
        inner.writeByte(Headers.NumberHeader)
        inner.writeDouble(d)
      case Value.String(s) =>
        inner.writeByte(Headers.StringHeader)
        inner.writeUTF(s)
      case Table(entries) =>
        inner.writeByte(Headers.TableHeader)
        inner.writeShort(entries.size)
        for ((k, v) <- entries) {
          writeData(k)
          writeData(v)
        }
    }

    inner.write(document.hash.bytes)
    inner.writeUTF(document.title)
    writeData(document.content)
    inner.close()
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
