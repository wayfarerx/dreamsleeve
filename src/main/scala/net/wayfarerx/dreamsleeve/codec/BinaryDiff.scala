package net.wayfarerx.dreamsleeve.codec

import java.io._
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import net.wayfarerx.dreamsleeve.model.{Diff, Hash, Headers, Node}

/**
 * The codec for reading and writing diffs as binary blobs.
 */
object BinaryDiff {

  import Node._
  import Diff._

  /** The header that identifies binary diffs. */
  private val Header = HeaderMask | /* type = */ 0x20L | /* version = */ 0x01L

  /**
   * Reads a diff from a byte array.
   *
   * @param bytes   The bytes to read from.
   * @param builder The hash builder to use for data integrity validation.
   * @return The diff that was read from the byte array.
   */
  def readBytes(bytes: Array[Byte])(implicit builder: Hash.Builder): Document =
    read(new ByteArrayInputStream(bytes))

  /**
   * Reads a diff from an input stream.
   *
   * @param input   The input stream to read from.
   * @param builder The hash builder to use for data integrity validation.
   * @return The diff that was read from the input stream.
   */
  def read(input: InputStream)(implicit builder: Hash.Builder): Document = {
    val outer = new DataInputStream(input)
    val header = outer.readLong()
    if (header != Header) throw new MalformedDiff.InvalidHeader(header)
    val inner = new DataInputStream(new GZIPInputStream(outer))

    /* Reads a value node from the stream. */
    def readChange(header: Byte): Change = header match {
      case Headers.AddHeader =>
        BinaryDocument.readData(inner.readByte(), inner) match {
          case Right(data) => Change.Add(data)
          case Left(invalid) => throw new MalformedDiff.InvalidDataHeader(invalid)
        }
      case Headers.CopyHeader =>
        val bytes = new Array[Byte](32)
        inner.readFully(bytes)
        Change.Copy(Hash(bytes))
      case Headers.ReplaceHeader =>
        val bytes = new Array[Byte](32)
        inner.readFully(bytes)
        BinaryDocument.readData(inner.readByte(), inner) match {
          case Right(data) => Change.Replace(Hash(bytes), data)
          case Left(invalid) => throw new MalformedDiff.InvalidDataHeader(invalid)
        }
      case Headers.ModifyHeader =>
        val bytes = new Array[Byte](32)
        inner.readFully(bytes)

        ???
      case invalid =>
        throw new MalformedDiff.InvalidChangeHeader(invalid)

    }

    /* Reads a value node from the stream. */
    def readValue(header: Byte): Value = header match {
      case Headers.BooleanHeader =>
        Value.Boolean(inner.readBoolean())
      case Headers.NumberHeader =>
        Value.Number(inner.readDouble())
      case Headers.StringHeader =>
        Value.String(inner.readUTF())
      case invalid =>
        throw new MalformedDiff.InvalidChangeHeader(invalid)
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
    if (result.hash != hash) throw new MalformedDiff.InvalidHash(hash, result.hash)
    result
  }

  /**
   * Writes a diff to an output stream.
   *
   * @param diff    The diff to write to the output stream.
   * @param builder The hash builder to use for data integrity validation.
   * @return The diff written to a byte array.
   */
  def writeBytes(diff: Document)(implicit builder: Hash.Builder): Array[Byte] = {
    val output = new ByteArrayOutputStream()
    write(diff, output)
    output.toByteArray
  }

  /**
   * Writes a diff to an output stream.
   *
   * @param diff    The diff to write to the output stream.
   * @param output  The output stream to write to.
   * @param builder The hash builder to use for data integrity validation.
   */
  def write(diff: Document, output: OutputStream)(implicit builder: Hash.Builder): Unit = {
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

    inner.write(diff.hash.bytes)
    inner.writeUTF(diff.title)
    writeData(diff.content)
    inner.close()
  }

  /**
   * Base class for exceptions encountered while reading binary diff blobs.
   *
   * @param msg The message associated with this exception.
   */
  abstract class MalformedDiff(msg: String) extends RuntimeException(msg)

  /**
   * The exceptions encountered while reading binary diff blobs.
   */
  object MalformedDiff {

    /**
     * The exception thrown when an invalid diff header is encountered.
     *
     * @param found The invalid header that was found.
     */
    class InvalidHeader(found: Long) extends MalformedDiff(
      s"Invalid header, expected ${Header.toHexString} found ${found.toHexString}.")

    /**
     * The exception thrown when an invalid diff header is encountered.
     *
     * @param found The invalid diff header that was found.
     */
    class InvalidDiffHeader(found: Byte) extends MalformedDiff(
      s"Invalid diff header: ${found.toHexString}.")

    /**
     * The exception thrown when an invalid change header is encountered.
     *
     * @param found The invalid change header that was found.
     */
    class InvalidChangeHeader(found: Byte) extends MalformedDiff(
      s"Invalid change header: ${found.toHexString}.")

    /**
     * The exception thrown when an invalid data header is encountered.
     *
     * @param found The invalid change header that was found.
     */
    class InvalidDataHeader(found: Byte) extends MalformedDiff(
      s"Invalid change header: ${found.toHexString}.")

    /**
     * The exception thrown when an invalid diff hash is encountered.
     *
     * @param expected The diff hash that was expected.
     * @param found    The invalid diff hash that was found.
     */
    class InvalidHash(expected: Hash, found: Hash) extends MalformedDiff(
      s"Invalid diff hash, expected ${expected.toShortString} found ${found.toShortString}.")

  }

}
