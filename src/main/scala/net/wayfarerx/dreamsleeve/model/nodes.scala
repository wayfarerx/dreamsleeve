package net.wayfarerx.dreamsleeve.model

import scala.collection.immutable.ListMap

/**
 * The base type of all complete model objects.
 */
sealed trait Node {

  /** The cached hash of this node. */
  @volatile private var _hash: Option[Hash] = None

  /** Returns a 160-bit hash code for this node. */
  final def hash(builder: Hash.Builder = Hash.Builder()): Hash =
    _hash getOrElse {
      val hash = hashWith(builder)
      _hash = Some(hash)
      hash
    }

  /**
   * Appends this node to the specified hash builder.
   *
   * @param builder The hash builder to append to.
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
  case class Boolean(value: scala.Boolean) extends Value {

    /* Hash this value with the specified hash builder. */
    override protected def hashWith(builder: Hash.Builder) =
      builder.hashBoolean(value)

    /* Return the string version of this value. */
    override def toString() = value.toString

  }

  /**
   * Represents numerical values.
   *
   * @param value The underlying value.
   */
  case class Number(value: Double) extends Value {

    /* Hash this value with the specified hash builder. */
    override protected def hashWith(builder: Hash.Builder) =
      builder.hashDouble(value)

    /* Return the string version of this value. */
    override def toString() = value.toString

  }

  /**
   * Represents string values.
   *
   * @param value The underlying value.
   */
  case class String(value: java.lang.String) extends Value {

    /* Hash this value with the specified hash builder. */
    override protected def hashWith(builder: Hash.Builder) =
      builder.hashString(value)

    /* Return the string version of this value. */
    override def toString() = s""""${value.toString.replace(""""""", """\"""")}""""

  }

}

/**
 * Represents tables of nodes.
 *
 * @param value The entries in the underlying table.
 */
case class Table(entries: ListMap[Value, Node] = ListMap()) extends Node {

  /* Hash this table with the specified hash builder. */
  override def hashWith(builder: Hash.Builder) =
    builder.hashTable(entries flatMap { case (k, v) => Seq(k.hash(builder), v.hash(builder)) })

}
