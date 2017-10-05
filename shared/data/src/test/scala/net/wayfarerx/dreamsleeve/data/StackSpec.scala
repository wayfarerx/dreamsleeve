/*
 * StackSpec.scala
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

import org.scalatest._

/**
 * Test case for the stack overflow protection.
 */
class StackSpec extends FlatSpec with Matchers {

  "A naive recursive function" should "throw a stack overflow exception" in {
    a[StackOverflowError] should be thrownBy countFragments(StackSpec.StackOverflowTable1)
    a[StackOverflowError] should be thrownBy countChanges(StackSpec.StackOverflowModify1)
  }

  /**
   * Recurse down a fragment structure.
   *
   * @param fragment The fragment to recurse on.
   * @return The depth of the recursion performed.
   */
  def countFragments(fragment: Fragment): Int = fragment match {
    case Table(e) => countFragments(e.head._2) + 1
    case _ => 1
  }

  /**
   * Recurse down a change structure.
   *
   * @param change The change to recurse on.
   * @return The depth of the recursion performed.
   */
  def countChanges(change: Change): Int = change match {
    case Update.Modify(_, c) => countChanges(c.head._2) + 1
    case _ => 1
  }

}

/**
 * Shared deeply recursive structures to test with.
 */
object StackSpec {

  /** The depth of the recursive structures. */
  val Overflow = 15000

  /** A deeply recursive table structure. */
  val StackOverflowTable1: Table = stackOverflowTable(Value.Boolean())

  /** Another deeply recursive table structure. */
  val StackOverflowTable2: Table = stackOverflowTable(Value.Boolean(true))

  /** A deeply recursive modify structure. */
  val StackOverflowModify1: Update.Modify = stackOverflowModify(Value.Boolean())

  /** Another deeply recursive modify structure. */
  val StackOverflowModify2: Update.Modify = stackOverflowModify(Value.Boolean(true))

  /**
   * Creates a deeply recursive table structure.
   *
   * @param last The last fragment in the chain.
   * @return A deeply recursive table structure.
   */
  def stackOverflowTable(last: Value): Table = {
    var fragment: Fragment = last
    var table: Table = null
    for (i <- 1 to Overflow) {
      table = Table(Value.Number(i) -> fragment)
      fragment = table
    }
    table
  }

  /**
   * Creates a deeply recursive modify structure.
   *
   * @param last The last fragment in the chain.
   * @return A deeply recursive modify structure.
   */
  def stackOverflowModify(last: Value): Update.Modify = {
    var fragment: Fragment = last
    var change: Change = Update.Copy(last)
    var modify: Update.Modify = null
    for (i <- 1 to Overflow) {
      val table = Table(Value.Number(i) -> fragment)
      fragment = table
      modify = Update.Modify(table, Value.Number(i) -> change)
      change = modify
    }
    modify
  }

}
