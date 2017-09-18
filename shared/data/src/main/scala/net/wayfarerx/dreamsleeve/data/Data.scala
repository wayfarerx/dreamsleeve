/*
 * Data.scala
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

import reflect.ClassTag

import cats._
import free.Free
import Free.liftF
import cats.implicits._

/**
 * Base type for data elements.
 */
abstract class Data private[data] extends Hashable {

  /** Alias for the language that describes equality operations. */
  final protected type EqualsOperation[T] = Data.EqualsOperation[T]

  /** Alias for the language that describes stringify operations. */
  final protected type ToStringOperation[T] = Data.ToStringOperation[T]

  /** Alias for the tasks that make up equality operations. */
  final protected type EqualsTask[T] = Data.EqualsTask[T]

  /** Alias for the tasks that make up stringify operations. */
  final protected type ToStringTask[T] = Data.ToStringTask[T]

  /* Use the equality operation for comparisons. */
  final override def equals(that: Any): Boolean =
    calculateEquals(that).foldMap(EqualsTask.Interpreter) getOrElse false

  /* Derive the hash code from a prefix of the data hash. */
  final override def hashCode(): Int = {
    val h = Hash.getInternalRepresentation(hash)
    (h(0) & 0x000000FF) << 24 | (h(1) & 0x000000FF) << 16 | (h(2) & 0x000000FF) << 8 | h(3) & 0x000000FF
  }

  /* Use the stringify operation for to string. */
  final override def toString: String = {
    val builder = new StringBuilder
    calculateToString().foldMap(ToStringTask.interpreter(builder))
    builder.toString
  }

  /**
   * Calculate the equality operation for this data element.
   *
   * @param that The instance to test against.
   * @return The equality operation for this data element.
   */
  protected[data] def calculateEquals(that: Any): EqualsOperation[Boolean]

  /**
   * Calculate the stringify operation for this data element.
   *
   * @return The stringify operation for this data element.
   */
  protected[data] def calculateToString(): ToStringOperation[Unit]

  /**
   * Alias for the equals task factory.
   *
   * @return The alias for the equals task factory
   */
  final protected def EqualsTask: Data.EqualsTask.type = Data.EqualsTask

  /**
   * Alias for the stringify task factory.
   *
   * @return The alias for the stringify task factory
   */
  final protected def ToStringTask: Data.ToStringTask.type = Data.ToStringTask

}

/**
 * Types associated with data elements.
 */
object Data {

  /** The language that describes equality operations. */
  type EqualsOperation[T] = Free[EqualsTask, T]

  /** The language that describes stringify operations. */
  type ToStringOperation[T] = Free[ToStringTask, T]

  /**
   * Base class for equality tasks.
   *
   * @tparam T The type returned by this equality task.
   */
  sealed trait EqualsTask[T] {

    /**
     * Applies this task.
     *
     * @return The result of this equality task.
     */
    def apply(): Option[T]

  }

  /**
   * Declarations associated with equality tasks.
   */
  object EqualsTask {

    /**
     * Creates an interpreter for equality tasks.
     *
     * @return An interpreter for equality tasks.
     */
    val Interpreter: EqualsTask ~> Option = new (EqualsTask ~> Option) {
      override def apply[T](op: EqualsTask[T]): Option[T] = op()
    }

    /**
     * Creates an equality operation that returns the supplied instance if it is of the specified type.
     *
     * @param instance The instance to return.
     * @tparam T The type of the instance to return.
     * @return An equality operation that returns the supplied instance if it is of the specified type.
     */
    def ofType[T: ClassTag](instance: Any): EqualsOperation[T] =
      liftF[EqualsTask, T](new EqualsTask[T] {
        override def apply(): Option[T] = implicitly[ClassTag[T]].unapply(instance)
      })

    /**
     * Creates an equality operation that returns true if the specified instances are equal and nothing otherwise.
     *
     * @param first  The first object to test.
     * @param second The second object to test.
     * @tparam T The types of the objects to test.
     * @return An equality operation that returns true if the specified instances are equal and nothing otherwise.
     */
    def areEqual[T: Eq](first: T, second: T): EqualsOperation[Boolean] =
      liftF[EqualsTask, Boolean](new EqualsTask[Boolean] {
        override def apply(): Option[Boolean] = Some(implicitly[Eq[T]].eqv(first, second)) filter identity
      })

  }

  /**
   * Base class for stringify tasks.
   *
   * @tparam T The type returned by this stringify task.
   */
  sealed trait ToStringTask[T] {

    /**
     * Applies this task to a string builder.
     *
     * @param builder The string builder to write to.
     * @return The result of this task.
     */
    def apply(builder: StringBuilder): T

  }

  /**
   * Declarations associated with stringify tasks.
   */
  object ToStringTask {

    /** The separator between entry items. */
    private val EntrySeparator = '='

    /** The separator between sequence items. */
    private val SequenceSeparator = ','

    /**
     * Creates a stringify operation that emits nothing.
     *
     * @return A stringify operation that emits nothing.
     */
    def pure(): ToStringOperation[Unit] =
      Free.pure[ToStringTask, Unit]()

    /**
     * Creates a stringify operation that emits the beginning of an element's string.
     *
     * @param name The name of the element that is beginning.
     * @return A stringify operation that emits the beginning of an element's string.
     */
    def begin(name: String): ToStringOperation[Unit] =
      liftF[ToStringTask, Unit](BeginElement(name))

    /**
     * Creates a stringify operation that emits the end of an element's string.
     *
     * @return A stringify operation that emits the end of an element's string.
     */
    def end(): ToStringOperation[Unit] =
      liftF[ToStringTask, Unit](EndElement)

    /**
     * Creates a stringify operation that emits the beginning of a map's string.
     *
     * @return A stringify operation that emits the beginning of a map's string.
     */
    def beginMap(): ToStringOperation[Unit] =
      liftF[ToStringTask, Unit](BeginMap)

    /**
     * Creates a stringify operation that emits the end of a map's string.
     *
     * @return A stringify operation that emits the end of a map's string.
     */
    def endMap(): ToStringOperation[Unit] =
      liftF[ToStringTask, Unit](EndMap)

    /**
     * Creates a stringify operation that emits the beginning of an entry's string.
     *
     * @return A stringify operation that emits the beginning of an entry's string.
     */
    def beginEntry(): ToStringOperation[Unit] =
      liftF[ToStringTask, Unit](BeginEntry)

    /**
     * Creates a stringify operation that emits the end of an entry's string.
     *
     * @return A stringify operation that emits the end of an entry's string.
     */
    def endEntry(): ToStringOperation[Unit] =
      liftF[ToStringTask, Unit](EndEntry)

    /**
     * Creates a stringify operation that emits a boolean value.
     *
     * @param value The value of the boolean to emit.
     * @return A stringify operation that emits a boolean value.
     */
    def emit(value: Boolean): ToStringOperation[Unit] =
      liftF[ToStringTask, Unit](EmitBoolean(value))

    /**
     * Creates a stringify operation that emits a double value.
     *
     * @param value The value of the double to emit.
     * @return A stringify operation that emits a double value.
     */
    def emit(value: Double): ToStringOperation[Unit] =
      liftF[ToStringTask, Unit](EmitDouble(value))

    /**
     * Creates a stringify operation that emits a string value.
     *
     * @param value The value of the string to emit.
     * @return A stringify operation that emits a string value.
     */
    def emit(value: String): ToStringOperation[Unit] =
      liftF[ToStringTask, Unit](EmitString(value))

    /**
     * Creates a stringify operation that emits a hash value.
     *
     * @param value The value of the hash to emit.
     * @return A stringify operation that emits a hash value.
     */
    def emit(value: Hash): ToStringOperation[Unit] =
      liftF[ToStringTask, Unit](EmitHash(value))

    /**
     * Creates an interpreter for stringify tasks.
     *
     * @param builder The string builder to append to.
     * @return An interpreter for stringify tasks.
     */
    def interpreter(builder: StringBuilder): ToStringTask ~> Id = new (ToStringTask ~> Id) {
      var beginning = true
      var separators = List[Char]()

      override def apply[T](op: ToStringTask[T]): T = {
        op match {
          case Begin(separator) =>
            if (!beginning) builder.append(separators.head)
            beginning = true
            separators ::= separator
          case End() =>
            beginning = false
            separators = separators.tail
          case _ =>
            if (!beginning) builder.append(separators.head)
            beginning = false
        }
        op(builder)
      }

    }

    /**
     * Base class for tasks that start a collection of items.
     *
     * @tparam T The type returned by this stringify task.
     */
    sealed trait Begin[T] extends ToStringTask[T] {

      /** The character used to separate items. */
      def separator: Char

    }

    /**
     * Extractor for begin tasks.
     */
    object Begin {

      /**
       * Extracts a begin task.
       *
       * @param task The task to extract.
       * @return The separator of the supplied task.
       */
      def unapply(task: Begin[_]): Option[Char] = Some(task.separator)

    }

    /**
     * Base class for tasks that finish a collection of items.
     *
     * @tparam T The type returned by this stringify task.
     */
    sealed trait End[T] extends ToStringTask[T]

    /**
     * Extractor for end tasks.
     */
    object End {

      /**
       * Extracts an end task.
       *
       * @param task The task to extract.
       * @return True.
       */
      def unapply(task: End[_]): Boolean = true

    }

    /**
     * Task that marks the beginning of a named sequence of items.
     *
     * @param name The name of the sequence.
     */
    case class BeginElement(name: String) extends Begin[Unit] {
      override def separator: Char = SequenceSeparator

      override def apply(builder: StringBuilder): Unit = {
        builder.append(name)
        builder.append('(')
      }
    }

    /**
     * Task that marks the end of a named sequence of items.
     */
    case object EndElement extends End[Unit] {
      override def apply(builder: StringBuilder): Unit = {
        builder.append(')')
      }
    }

    /**
     * Task that marks the beginning of a sequence of entries.
     */
    case object BeginMap extends Begin[Unit] {
      override def separator: Char = SequenceSeparator

      override def apply(builder: StringBuilder): Unit = {
        builder.append('{')
      }
    }

    /**
     * Task that marks the end of a sequence of entries.
     */
    case object EndMap extends End[Unit] {
      override def apply(builder: StringBuilder): Unit = {
        builder.append('}')
      }
    }

    /**
     * Task that marks the beginning of a single entry.
     */
    case object BeginEntry extends Begin[Unit] {
      override def separator: Char = EntrySeparator

      override def apply(builder: StringBuilder): Unit = ()
    }

    /**
     * Task that marks the end of a single entry.
     */
    case object EndEntry extends End[Unit] {
      override def apply(builder: StringBuilder): Unit = ()
    }

    /**
     * Task that emits a single boolean value.
     *
     * @param value The value to emit.
     */
    case class EmitBoolean(value: Boolean) extends ToStringTask[Unit] {
      override def apply(builder: StringBuilder): Unit = {
        builder.append(value)
      }
    }

    /**
     * Task that emits a single double value.
     *
     * @param value The value to emit.
     */
    case class EmitDouble(value: Double) extends ToStringTask[Unit] {
      override def apply(builder: StringBuilder): Unit = {
        val floor = value.floor
        if (value == floor) builder.append(floor.toLong) else builder.append(value)
      }
    }

    /**
     * Task that emits a single string value.
     *
     * @param value The value to emit.
     */
    case class EmitString(value: String) extends ToStringTask[Unit] {
      override def apply(builder: StringBuilder): Unit = {
        builder.append(value)
      }
    }

    /**
     * Task that emits a single hash value.
     *
     * @param value The value to emit.
     */
    case class EmitHash(value: Hash) extends ToStringTask[Unit] {
      override def apply(builder: StringBuilder): Unit = {
        builder.append(value)
      }
    }

  }

}