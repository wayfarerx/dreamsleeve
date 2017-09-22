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
  override protected[data] def calculateEquals(that: Any): Eval[Boolean] = that match {
    case Document(thatTitle, thatContent) if thatTitle == title => content.calculateEquals(thatContent)
    case _ => Eval.False
  }

  /* Calculate the hash for this document. */
  override protected def calculateHash(generator: Hash.Generator): Eval[Hash] =
    for (c <- content.evalHash(generator)) yield generator.hash(Document.Header, title, c)

  /* Calculate the string for this document. */
  override protected[data] def calculateToString(): Eval[String] =
    for (c <- content.calculateToString()) yield s"Document($title,$c)"

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
    override protected[data] def calculateEquals(that: Any): Eval[scala.Boolean] = that match {
      case Boolean(v) if v == value => Eval.True
      case _ => Eval.False
    }

    /* Calculate the hash for this boolean value. */
    override protected def calculateHash(generator: Hash.Generator): Eval[Hash] =
      Eval.now(generator.hash(Boolean.Header, value))

    /* Calculate the string for this boolean value. */
    override protected[data] def calculateToString(): Eval[java.lang.String] =
      Eval.now(s"Boolean($value)")

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
    override protected[data] def calculateEquals(that: Any): Eval[scala.Boolean] = that match {
      case Number(v) if v == value => Eval.True
      case _ => Eval.False
    }

    /* Calculate the hash for this number value. */
    override protected def calculateHash(generator: Hash.Generator): Eval[Hash] =
      Eval.now(generator.hash(Number.Header, value))

    /* Calculate the string for this number value. */
    override protected[data] def calculateToString(): Eval[java.lang.String] = {
      val floor = value.floor
      if (floor == value) Eval.now(s"Number(${value.toLong})")
      else Eval.now(s"Number($value)")
    }

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
    override protected[data] def calculateEquals(that: Any): Eval[scala.Boolean] = that match {
      case String(v) if v == value => Eval.True
      case _ => Eval.False
    }

    /* Calculate the hash for this string value. */
    override protected def calculateHash(generator: Hash.Generator): Eval[Hash] =
      Eval.now(generator.hash(String.Header, value))

    /* Calculate the string for this string value. */
    override protected[data] def calculateToString(): Eval[java.lang.String] =
      Eval.now(s"String($value)")

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
  override protected[data] def calculateEquals(that: Any): Eval[Boolean] = that match {
    case Table(thatEntries) if thatEntries.size == entries.size =>
      (Eval.True /: entries.flatMap(e => Seq(e._1, e._2)).zip(thatEntries.flatMap(e => Seq(e._1, e._2)))) { (r, e) =>
        val (e1, e2) = e
        r flatMap { rr => if (rr) e1.calculateEquals(e2) else Eval.False }
      }
    case _ => Eval.False
  }

  /* Calculate the hash for this table. */
  override protected def calculateHash(generator: Hash.Generator): Eval[Hash] = for {
    ee <- (Eval.now(Vector[Hash]()) /: entries) { (r, e) =>
      for {
        rr <- r
        (k, v) = e
        kk <- k.evalHash(generator)
        vv <- v.evalHash(generator)
      } yield rr :+ kk :+ vv
    }
  } yield generator.hash(Table.Header, ee)

  /* Calculate the string for this table. */
  override protected[data] def calculateToString(): Eval[String] = {
    (Eval.now("") /: entries) { (r, e) =>
      for {
        rr <- r
        (k, v) = e
        kk <- k.calculateToString()
        vv <- v.calculateToString()
      } yield if (rr.isEmpty) s"$kk=$vv" else s"$rr,$kk=$vv"
    } map (c => s"Table($c)")
  }

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
