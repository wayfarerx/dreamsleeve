package net.wayfarerx.dreamsleeve.model

import scala.collection.immutable.ListMap

/**
 * The base type of all complete model objects.
 */
sealed trait Node {

  /** The cached hash of this node. */
  @volatile private var _hash: Option[Hash] = None

  /**
   * Returns a hash for this node.
   *
   * @return A hash for this node.
   */
  final def hash(): Hash =
    _hash getOrElse generateHash(Hash.Builder())

  /**
   * Returns a hash for this node.
   *
   * @param builder The hash builder to use.
   * @return A hash for this node.
   */
  final def hash(builder: Hash.Builder): Hash =
    _hash getOrElse generateHash(builder)

  /**
   * Generates and stores a hash for this node.
   *
   * @param builder The hash builder to use.
   * @return A hash for this node.
   */
  private def generateHash(builder: Hash.Builder): Hash = {
    val hash = hashWith(builder)
    _hash = Some(hash)
    hash
  }

  /**
   * Generates a hash for this node.
   *
   * @param builder The hash builder to use.
   * @return A hash for this node.
   */
  protected def hashWith(builder: Hash.Builder): Hash

}

/**
 * The base type of all complete model objects that represent a single value.
 */
sealed trait Value extends Node

/**
 * Implementations of the value types.
 */
object Value {

  /**
   * Represents true or false values.
   *
   * @param value The underlying value.
   */
  case class Boolean(value: scala.Boolean = false) extends Value {

    /* Hash this value with the specified hash builder. */
    override protected def hashWith(builder: Hash.Builder) =
      builder.hashBoolean(value)

  }

  /**
   * Represents numerical values.
   *
   * @param value The underlying value.
   */
  case class Number(value: Double = 0.0) extends Value {

    /* Hash this value with the specified hash builder. */
    override protected def hashWith(builder: Hash.Builder) =
      builder.hashDouble(value)

  }

  /**
   * Represents string values.
   *
   * @param value The underlying value.
   */
  case class String(value: java.lang.String = "") extends Value {

    /* Hash this value with the specified hash builder. */
    override protected def hashWith(builder: Hash.Builder) =
      builder.hashString(value)

  }

}

/**
 * Represents tables of nodes indexed by value.
 *
 * @param entries The entries in the underlying table.
 */
case class Table(entries: ListMap[Value, Node] = ListMap()) extends Node {

  /* Hash this table with the specified hash builder. */
  override def hashWith(builder: Hash.Builder) =
    builder.hashTable(entries flatMap { case (k, v) => Seq(k.hash(builder), v.hash(builder)) })

}

/**
 * Represents document of nodes.
 *
 * @param title The title of the document.
 * @param content The content of the document.
 */
case class Document(title: String, content: Node) extends Node {

  /* Hash this table with the specified hash builder. */
  override def hashWith(builder: Hash.Builder) =
    builder.hashDocument(title, content.hash(builder))

}
