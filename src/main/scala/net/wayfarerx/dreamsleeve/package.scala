/*
 * package.scala
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

package net.wayfarerx

import cats.Monad

/**
 * Global definitions used by the various dreamsleeve projects.
 */
package object dreamsleeve {

  /**
   * The monad implementation for problematic operation results.
   *
   * @tparam ProblemType The type of problem that may be encountered.
   */
  private[dreamsleeve] final class ProblematicMonad[ProblemType] extends Monad[Either[ProblemType, ?]] {

    /* Right-dominant flat mapping. */
    override def flatMap[A, B](fa: Either[ProblemType, A])(f: A => Either[ProblemType, B]): Either[ProblemType, B] =
      fa.flatMap(f)

    /* Right-dominant pure. */
    override def pure[A](x: A): Either[ProblemType, A] = Right(x)

    /* Right-dominant recursion. */
    @annotation.tailrec
    override def tailRecM[A, B](a: A)(f: A => Either[ProblemType, Either[A, B]]): Either[ProblemType, B] =
    f(a) match {
      case Right(Right(b)) => Right(b)
      case Right(Left(a)) => tailRecM(a)(f)
      case l@Left(_) => l.asInstanceOf[Left[ProblemType, B]]
    }

  }

}
