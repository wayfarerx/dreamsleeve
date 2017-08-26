/*
 * BinaryContext.scala
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

package net.wayfarerx.dreamsleeve.io

import java.nio.ByteBuffer

/**
 * Base class for the context that all binary IO operations execute in.
 */
trait BinaryContext {

  /**
   * Acquires a buffer from this context. NOTE: this method may return the same buffer on each invocation.
   *
   * @param size The number of bytes the buffer should contain.
   * @return A buffer from this context.
   */
  def acquireBytes(size: Int): ByteBuffer

}

/**
 * Factory for contextual binary IO operations.
 */
object BinaryContext {

  /**
   * Support for the context interface.
   */
  abstract class Support extends BinaryContext {

    /** The most recently constructed buffer object. */
    private var buffer = ByteBuffer.allocate(16)

    /* Return the existing buffer if it is big enough, otherwise create a new buffer. */
    final override def acquireBytes(size: Int): ByteBuffer = {
      if (buffer.capacity() < size) buffer = ByteBuffer.allocate(size) else {
        buffer.position(0)
        buffer.limit(size)
      }
      buffer
    }

  }

}
