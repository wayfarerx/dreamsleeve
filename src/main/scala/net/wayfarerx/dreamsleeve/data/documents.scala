/*
 * documents.scala
 *
 * Copyright 2017 wayfarerx <x@wayfarerx.net> (@thewayfarerx)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.wayfarerx.dreamsleeve.data

import scala.collection.immutable.{SortedMap, SortedSet}

/**
 * Represents a data document.
 *
 * @param title   The title of the document.
 * @param content The content of the document.
 */
case class Document(title: String, content: Fragment) extends Hashable {

  /* Generate the hash for this document. */
  override private[data] def generateHash(implicit hasher: Hasher): Hash =
    hasher.hashDocument(title, content.hash)

}

/**
 * Declarations associated with documents.
 */
object Document {

  /** The header for documents. */
  val Header: Byte = 0xE1.toByte

}

/**
 * The base type of all document fragments.
 */
sealed trait Fragment extends Hashable

/**
 * Extractor for fragment implementations.
 */
object Fragment {

  /**
   * Extracts any fragment implementation.
   *
   * @param fragment The fragment to extract.
   * @return True for every fragment.
   */
  def unapply(fragment: Fragment): Boolean =
    true

}

/**
 * Represents tables of fragments indexed by a value.
 *
 * @param entries The entries in the underlying table.
 */
case class Table(entries: SortedMap[Value, Fragment]) extends Fragment {

  /** The set of keys in this table. */
  lazy val keys: SortedSet[Value] = entries.keySet

  /** The set of values in this table. */
  lazy val values: Iterable[Fragment] = entries.values

  /**
   * Returns a fragment in this table.
   *
   * @param key The key to return the fragment for.
   * @return A fragment in this table.
   */
  def apply(key: Value): Fragment =
    entries(key)

  /**
   * Attempts to return a fragment in this table.
   *
   * @param key The key to return the fragment for.
   * @return A fragment if one exists in this table for the specified key.
   */
  def get(key: Value): Option[Fragment] =
    entries get key

  /* Generate the hash for this table. */
  override private[data] def generateHash(implicit hasher: Hasher): Hash =
    hasher.hashTable(entries flatMap { case (k, v) => Seq(k.hash, v.hash) })

}

/**
 * Factory for tables.
 */
object Table {

  /** The header for tables. */
  val Header: Byte = 0xD2.toByte

  /**
   * Creates a new table.
   *
   * @param entries The items to populate the table with.
   * @return A new table populated with the specified entries.
   */
  def apply(entries: (Value, Fragment)*): Table =
    Table(SortedMap(entries: _*))

}

/**
 * The base type of all fragments that represent a single value.
 */
sealed trait Value extends Fragment with Comparable[Value]

/**
 * Implementations of the value types.
 */
object Value {

  /**
   * Extracts any value implementation.
   *
   * @param value The value to extract.
   * @return True for every value.
   */
  def unapply(value: Value): scala.Boolean =
    true

  /**
   * Represents true or false values.
   *
   * @param value The underlying value.
   */
  case class Boolean(value: scala.Boolean = false) extends Value {

    /* Generate the hash for this boolean value. */
    override private[data] def generateHash(implicit hasher: Hasher): Hash =
      hasher.hashBoolean(value)

    /* Compare this value with another value. */
    override def compareTo(that: Value): Int = that match {
      case Boolean(b) => if (b == value) 0 else if (b) -1 else 1
      case _ => -1
    }

  }

  /**
   * Declarations associated with booleans.
   */
  object Boolean {

    /** The header for booleans. */
    val Header: Byte = 0xC3.toByte

  }

  /**
   * Represents numerical values.
   *
   * @param value The underlying value.
   */
  case class Number(value: Double = 0.0) extends Value {

    /* Generate the hash for this number value. */
    override private[data] def generateHash(implicit hasher: Hasher): Hash =
      hasher.hashNumber(value)

    /* Compare this value with another value. */
    override def compareTo(that: Value): Int = that match {
      case Boolean(_) => 1
      case Number(n) => if (n == value) 0 else if (n > value) -1 else 1
      case _ => -1
    }

  }

  /**
   * Declarations associated with numbers.
   */
  object Number {

    /** The header for numbers. */
    val Header: Byte = 0xB4.toByte

  }

  /**
   * Represents string values.
   *
   * @param value The underlying value.
   */
  case class String(value: java.lang.String = "") extends Value {

    /* Generate the hash for this string value. */
    override private[data] def generateHash(implicit hasher: Hasher): Hash =
      hasher.hashString(value)

    /* Compare this value with another value. */
    override def compareTo(that: Value): Int = that match {
      case String(s) => value.compareTo(s)
      case _ => 1
    }

  }

  /**
   * Declarations associated with strings.
   */
  object String {

    /** The header for strings. */
    val Header: Byte = 0xA5.toByte

  }

}