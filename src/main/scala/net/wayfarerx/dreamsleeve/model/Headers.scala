package net.wayfarerx.dreamsleeve.model

/**
 * Created by wayfarerx on 5/29/2017.
 */
object Headers {

  /** The header for documents. */
  val DocumentHeader = 0xE1.toByte
  /** The header for tables. */
  val TableHeader = 0xD2.toByte
  /** The header for strings. */
  val StringHeader = 0xC3.toByte
  /** The header for numbers. */
  val NumberHeader = 0xB4.toByte
  /** The header for booleans. */
  val BooleanHeader = 0xA5.toByte
  /** The header for document creation. */
  val CreateHeader = 0x96.toByte
  /** The header for document revisions. */
  val ReviseHeader = 0x87.toByte
  /** The header for document removal. */
  val RemoveHeader = 0x78.toByte
  /** The header for add operations. */
  val AddHeader = 0x69.toByte
  /** The header for copy operations. */
  val CopyHeader = 0x5A.toByte
  /** The header for replace operations. */
  val ReplaceHeader = 0x4B.toByte
  /** The header for modify operations. */
  val ModifyHeader = 0x3C.toByte
  /** The header for delete operations. */
  val DeleteHeader = 0x2D.toByte

}
