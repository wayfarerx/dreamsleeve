package net.wayfarerx.dreamsleeve.model

import collection.immutable.{SortedMap, SortedSet}

/**
 * The base type of all model nodes.
 */
sealed trait Node extends Hash.Support

/**
 * Implementations of the various model nodes.
 */
object Node {

  /**
   * The base type of all model nodes that contain actual data.
   */
  sealed trait Data extends Node

  /**
   * The base type of all model nodes that represent a single value.
   */
  sealed trait Value extends Data with Comparable[Value]

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
      override protected def generateHash(implicit builder: Hash.Builder): Hash =
        builder.hashBoolean(value)

      /* Compare this node with another node. */
      override def compareTo(that: Value): Int = that match {
        case Boolean(b) => if (b == value) 0 else if (b) -1 else 1
        case _ => -1
      }

    }

    /**
     * Represents numerical values.
     *
     * @param value The underlying value.
     */
    case class Number(value: Double = 0.0) extends Value {

      /* Hash this value with the specified hash builder. */
      override protected def generateHash(implicit builder: Hash.Builder): Hash =
        builder.hashNumber(value)

      /* Compare this node with another node. */
      override def compareTo(that: Value): Int = that match {
        case Boolean(_) => 1
        case Number(n) => if (n == value) 0 else if (n > value) -1 else 1
        case _ => -1
      }

    }

    /**
     * Represents string values.
     *
     * @param value The underlying value.
     */
    case class String(value: java.lang.String = "") extends Value {

      /* Hash this value with the specified hash builder. */
      override protected def generateHash(implicit builder: Hash.Builder): Hash =
        builder.hashString(value)

      /* Compare this node with another node. */
      override def compareTo(that: Value): Int = that match {
        case String(s) => value.compareTo(s)
        case _ => 1
      }

    }

  }

  /**
   * Represents tables of nodes indexed by value.
   *
   * @param entries The entries in the underlying table.
   */
  case class Table(entries: SortedMap[Value, Data] = SortedMap()) extends Data {

    /** The list of keys in this table. */
    lazy val keys: SortedSet[Value] = entries.keySet

    /**
     * Returns a value in this table.
     *
     * @param key The key to return the value for.
     * @return A value in this table.
     */
    def apply(key: Value): Data =
      entries(key)

    /**
     * Attempts to return a value in this table.
     *
     * @param key The key to return the value for.
     * @return A value if one exists in this table for the specified key.
     */
    def get(key: Value): Option[Data] =
      entries get key

    /* Hash this table with the specified hash builder. */
    override protected def generateHash(implicit builder: Hash.Builder): Hash =
      builder.hashTable(entries flatMap { case (k, v) => Seq(k.hash, v.hash) })

  }

  /**
   * Factory for tables.
   */
  object Table {

    /**
     * Creates a new table.
     *
     * @param items The items to populate the table with.
     * @return A new table populated with the specified data.
     */
    def apply(items: (Value, Data)*): Table =
      Table(SortedMap(items: _*))

  }

  /**
   * Represents document of nodes.
   *
   * @param title   The title of the document.
   * @param content The content of the document.
   */
  case class Document(title: String, content: Data) extends Node {

    /* Hash this table with the specified hash builder. */
    override protected def generateHash(implicit builder: Hash.Builder): Hash =
      builder.hashDocument(title, content.hash)

  }


}
