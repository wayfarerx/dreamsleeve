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

  /**
   * Computes the revise that transforms this document into the specified document.
   *
   * @param to     The document to compute the revise against.
   * @param hasher The hasher to generate hashes with.
   * @return The revise that transforms this document into the specified document.
   */
  def computeRevise(to: Document)(implicit hasher: Hasher): Difference.Revise =
    Difference.Revise(this, to.title, content.computeUpdate(to.content))

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
sealed trait Fragment extends Hashable {

  /**
   * Computes the update that transforms this fragment into the specified fragment.
   *
   * @param to     The fragment to compute the update against.
   * @param hasher The hasher to generate hashes with.
   * @return The update that transforms this fragment into the specified fragment.
   */
  def computeUpdate(to: Fragment)(implicit hasher: Hasher): Change.Update

}

/**
 * Represents tables of fragments indexed by a value.
 *
 * @param entries The entries in the underlying table.
 */
case class Table(entries: SortedMap[Value, Fragment]) extends Fragment {

  /** The set of keys in this table. */
  lazy val keys: SortedSet[Value] = entries.keySet

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

  /* Compute the update as a copy if identical, a modify if a different table and a replace otherwise. */
  override def computeUpdate(to: Fragment)(implicit hasher: Hasher): Change.Update = to match {
    case toFragment if toFragment == this => Change.Copy(this)
    case toTable@Table(_) => Change.Modify(this, (keys ++ toTable.keys).toSeq map { k =>
      get(k) -> toTable.get(k) match {
        case (None, Some(tf)) => k -> Change.Add(tf)
        case (Some(ff), Some(tf)) => k -> ff.computeUpdate(tf)
        case (Some(ff), None) => k -> Change.Remove(ff)
        case (None, None) => sys.error("unreachable")
      }
    }: _*)
    case _ => Change.Replace(hash, to)
  }

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
sealed trait Value extends Fragment with Comparable[Value] {

  /* Compute the update as a copy if identical and a replace if not. */
  final override def computeUpdate(to: Fragment)(implicit hasher: Hasher): Change.Update =
    if (to == this) Change.Copy(hash)
    else Change.Replace(hash, to)

}

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