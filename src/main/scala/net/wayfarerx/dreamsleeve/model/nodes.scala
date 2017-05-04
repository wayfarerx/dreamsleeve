package net.wayfarerx.dreamsleeve.model

import scala.collection.immutable.ListMap

/**
 * The base type of all complete model objects.
 */
sealed trait Node extends Hash.Support

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
      builder.hashNumber(value)

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
  override protected def hashWith(builder: Hash.Builder) =
    builder.hashTable(entries flatMap { case (k, v) => Seq(k.hash(builder), v.hash(builder)) })

}

/**
 * Represents document of nodes.
 *
 * @param title The title of the document.
 * @param content The content of the document.
 */
case class Document(title: String, content: Node) extends Hash.Support {

  /* Hash this table with the specified hash builder. */
  override protected def hashWith(builder: Hash.Builder) =
    builder.hashDocument(title, content.hash(builder))

}
