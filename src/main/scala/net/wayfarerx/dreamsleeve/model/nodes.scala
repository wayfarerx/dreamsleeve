package net.wayfarerx.dreamsleeve.model

import scala.collection.immutable.ListMap

/**
 * The base type of all complete model objects.
 */
sealed trait Node {

  /** Returns a 160-bit hash code for this node. */
  final lazy val hash: Hash = {
    val builder = Hash.Builder()
    hashWith(builder)
    builder.complete()
  }

  /**
   * Appends this node to the specified hash builder.
   *
   * @param builder The hash builder to append to.
   */
  def hashWith(builder: Hash.Builder): Unit

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
    import Boolean._

    /* Append this value to the specified hash builder. */
    override def hashWith(builder: Hash.Builder) =
      builder.appendBoolean(value)

    /* Return the string version of this value. */
    override def toString() = value.toString

  }

  /**
   * Represents numerical values.
   *
   * @param value The underlying value.
   */
  case class Number(value: Double) extends Value {

    /* Append this value to the specified hash builder. */
    override def hashWith(builder: Hash.Builder) =
      builder.appendDouble(value)

    /* Return the string version of this value. */
    override def toString() = value.toString

  }

  /**
   * Represents string values.
   *
   * @param value The underlying value.
   */
  case class String(value: java.lang.String) extends Value {

    /* Append this value to the specified hash builder. */
    override def hashWith(builder: Hash.Builder) =
      builder.appendString(value)

    /* Return the string version of this value. */
    override def toString() = value.toString

  }

}

/**
 * Represents tables of nodes.
 */
case class Table(entries: ListMap[Value, Node] = ListMap()) extends Node {

  /* Append this table to the specified hash builder. */
  override def hashWith(builder: Hash.Builder) =
    builder.appendTable {
      entries foreach {
        case (key, value) =>
          key.hashWith(builder)
          value.hashWith(builder)
      }
    }

}
