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

import collection.immutable.{SortedMap, SortedSet}

import cats.Eval

/**
 * Represents a data document.
 *
 * @param title   The title of the document.
 * @param content The content of the document.
 */
case class Document(title: String, content: Fragment) extends Data {

  /* Test for equality with this document. */
  override protected def calculateEquals(that: Any): Eval[Boolean] = that match {
    case Document(thatTitle, thatContent) if thatTitle == title => Eval.always(thatContent == content)
    case _ => Eval.now(false)
  }

  /* Calculate the string for this document. */
  override protected def calculateToString(): Eval[String] =
    for (c <- Eval.always(content.toString)) yield s"Document($title,$c)"

  /* Calculate the hash for this document. */
  override protected def calculateHash(): HashOperation[Hash] = for {
    c <- content.hashOperation
    h <- HashTask.hash(Document.Header, title, c)
  } yield h

}

/**
 * Declarations associated with documents.
 */
object Document extends
  binary_data.Documents with
  textual_data.TextualDocuments.Documents {

  /** The header for documents. */
  val Header: Byte = 0xE1.toByte

}

/**
 * The base type of all document fragments.
 */
sealed abstract class Fragment extends Data

/**
 * Extractor for fragment implementations.
 */
object Fragment extends
  binary_data.Fragments with
  textual_data.TextualFragments.Fragments {

  /**
   * Extracts any fragment implementation.
   *
   * @param fragment The fragment to extract.
   * @return Eval.now(true) for every fragment.
   */
  def unapply(fragment: Fragment): Boolean =
    true

}

/**
 * The base type of all fragments that represent a single value.
 */
sealed abstract class Value extends Fragment with Comparable[Value]

/**
 * Implementations of the value types.
 */
object Value extends
  binary_data.Values with
  textual_data.TextualValues.Values {

  /**
   * Extracts any value implementation.
   *
   * @param value The value to extract.
   * @return Eval.now(true) for every value.
   */
  def unapply(value: Value): scala.Boolean =
    true

  /**
   * Represents true or false values.
   *
   * @param value The underlying value.
   */
  case class Boolean(value: scala.Boolean = false) extends Value {

    /* Test for equality with this boolean value. */
    override protected def calculateEquals(that: Any): Eval[scala.Boolean] = that match {
      case Boolean(v) if v == value => Eval.now(true)
      case _ => Eval.now(false)
    }

    /* Calculate the string for this boolean value. */
    override protected def calculateToString(): Eval[java.lang.String] =
      Eval.now(s"Boolean($value)")

    /* Calculate the hash for this boolean value. */
    override protected def calculateHash(): HashOperation[Hash] = for {
      h <- HashTask.hash(Boolean.Header, value)
    } yield h

    /* Compare this value with another value. */
    override def compareTo(that: Value): Int = that match {
      case Boolean(b) => if (b == value) 0 else if (b) -1 else 1
      case _ => -1
    }

  }

  /**
   * Declarations associated with booleans.
   */
  object Boolean extends
    binary_data.Booleans with
    textual_data.TextualValues.Booleans {

    /** The header for booleans. */
    val Header: Byte = 0xC3.toByte

  }

  /**
   * Represents numerical values.
   *
   * @param value The underlying value.
   */
  case class Number(value: Double = 0.0) extends Value {

    /* Test for equality with this number value. */
    override protected def calculateEquals(that: Any): Eval[scala.Boolean] = that match {
      case Number(v) if v == value => Eval.now(true)
      case _ => Eval.now(false)
    }

    /* Calculate the string for this number value. */
    override protected def calculateToString(): Eval[java.lang.String] = {
      val floor = value.floor
      if (floor == value) Eval.now(s"Number(${value.toLong})")
      else Eval.now(s"Number($value)")
    }

    /* Calculate the hash for this number value. */
    override protected def calculateHash(): HashOperation[Hash] = for {
      h <- HashTask.hash(Number.Header, value)
    } yield h

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
  object Number extends
    binary_data.Numbers with
    textual_data.TextualValues.Numbers {

    /** The header for numbers. */
    val Header: Byte = 0xB4.toByte

  }

  /**
   * Represents string values.
   *
   * @param value The underlying value.
   */
  case class String(value: java.lang.String = "") extends Value {

    /* Test for equality with this string value. */
    override protected def calculateEquals(that: Any): Eval[scala.Boolean] = that match {
      case String(v) if v == value => Eval.now(true)
      case _ => Eval.now(false)
    }

    /* Calculate the string for this string value. */
    override protected def calculateToString(): Eval[java.lang.String] =
      Eval.now(s"String($value)")

    /* Calculate the hash for this string value. */
    override protected def calculateHash(): HashOperation[Hash] = for {
      h <- HashTask.hash(String.Header, value)
    } yield h

    /* Compare this value with another value. */
    override def compareTo(that: Value): Int = that match {
      case String(s) => value.compareTo(s)
      case _ => 1
    }

  }

  /**
   * Declarations associated with strings.
   */
  object String extends
    binary_data.Strings with
    textual_data.TextualValues.Strings {

    /** The header for strings. */
    val Header: Byte = 0xA5.toByte

  }

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

  /* Test for equality with this table. */
  override protected def calculateEquals(that: Any): Eval[Boolean] = that match {
    case Table(thatEntries) => Eval.always(thatEntries == entries)
    case _ => Eval.now(false)
  }

  /* Calculate the string for this table. */
  override protected def calculateToString(): Eval[String] =
    for (e <- Eval.always(entries.map(e => s"${e._1}=${e._2}").mkString(","))) yield s"Table($e)"

  /* Calculate the hash for this table. */
  override protected def calculateHash(): HashOperation[Hash] = for {
    ee <- (HashTask.pure(Vector[Hash]()) /: entries) { (r, e) =>
      for {
        rr <- r
        (k, v) = e
        kk <- k.hashOperation
        vv <- v.hashOperation
      } yield rr :+ kk :+ vv
    }
    h <- HashTask.hash(Table.Header, ee)
  } yield h

}

/**
 * Factory for tables.
 */
object Table extends
  binary_data.Tables with
  textual_data.TextualTables.Tables {

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
