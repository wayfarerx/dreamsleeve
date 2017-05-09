package net.wayfarerx.dreamsleeve
package diff

import java.util

/**
 * Performs a linear time and space comparison of two objects by comparing both objects in both directions to find a
 * overlapping path which is called the middle snake.
 * <p>
 * Myers proved that the middle segment is already a part of the solution. Furthermore the middle segment divides the
 * comparison in two sub problems, which further can be compared using this technique.
 *
 * @author Roman Vottner
 */
object LinearDiff {

  /**
   * Calculates the shortest sequence of edits that will transform the first sequence into the second.
   *
   * @param from The original sequence.
   * @param to   The resulting sequence.
   * @tparam T The type of element to diff.
   * @return The shortest sequence of edits that will transform the first sequence into the second.
   */
  def edits[T](from: Vector[model.Value], to: Vector[model.Value]): Vector[model.Edit] = {
    import scala.collection.JavaConverters._
    Option(Compare(from.toArray, to.toArray).getSnakes).toVector.flatMap(_.asScala) flatMap { snake =>
      if (snake.IsForward) {
        ???
      } else {
        ???
      }
    }
  }

  /**
   * Compares two character sequences or strings with each other and calculates the shortest edit sequence (SES) as
   * well as the longest common subsequence (LCS) to transfer input <em>a</em> to input <em>b</em>. The SES are the
   * necessary actions required to perform the transformation.
   *
   * @param a
   * The first character sequence; usually the oldest string
   * @param b
   * The second character sequence; usually the newest string
   * @return The result containing the snake that lead from input string a to input string b
   * @throws Exception
   */
  @throws[Exception]
  def CompareStrings(a: String, b: String): Results[String] = {
    val aa = StringToArray(a)
    val ab = StringToArray(b)
    Compare(aa, ab)
  }

  /**
   * Transforms a string into an array of single string characters.
   *
   * @param s
   * The string to turn into a string array
   * @return The input string transformed into an array of string characters
   */
  private def StringToArray(s: String) = {
    val as = new Array[String](s.length)
    var i = 0
    for (c <- s.toCharArray) {
      as({
        i += 1;
        i - 1
      }) = "" + c
    }
    as
  }

  /**
   * Compares two arrays of type <em>T</em> with each other and calculates the shortest edit sequence (SES) as well as
   * the longest common subsequence (LCS) to transfer input <em>a</em> to input <em>b</em>. The SES are the necessary
   * actions required to perform the transformation.
   *
   * @param aa
   * Usually the older object which should be compared
   * @param ab
   * Usually the newest object to be compared with <em>aa</em>
   * @return The result containing the snake that lead from input <em>aa</em> to input <em>ab</em>
   * @throws Exception
   */
  @throws[Exception]
  def Compare[T <: AnyRef](aa: Array[T], ab: Array[T]): Results[T] = {
    val VForward = new V(aa.length, ab.length, true, true)
    val VReverse = new V(aa.length, ab.length, false, true)
    val snakes = new util.ArrayList[Snake[T]]
    val forwardVs = new util.ArrayList[V]
    val reverseVs = new util.ArrayList[V]
    CompareSnakes(snakes, forwardVs, reverseVs, aa, aa.length, ab, ab.length, VForward, VReverse)
    new Results[T](snakes, forwardVs, reverseVs)
  }

  /**
   * Compares two arrays of type <em>T</em> with each other and calculates the shortest edit sequence (SES) as well as
   * the longest common subsequence (LCS) to transfer input <em>a</em> to input <em>b</em>. The SES are the necessary
   * actions required to perform the transformation.
   *
   * @param snakes
   * The possible solution paths for transforming object <em>pa </em> to <em>pb</em>
   * @param forwardVs
   * All saved end points in forward direction indexed on <em>d</em>
   * @param reverseVs
   * All saved end points in backward direction indexed on <em>d</em>
   * @param pa
   * Elements of the first object. Usually the original object
   * @param N
   * The number of elements of the first object to compare
   * @param pb
   * Elements of the second object. Usually the current object
   * @param M
   * The number of elements of the second object to compare
   * @param VForward
   * An array of end points for a given k-line in forward direction
   * @param VReverse
   * An array of end points for a given k-line in backward direction
   * @throws Exception
   */
  @throws[Exception]
  private[diff] def CompareSnakes[T <: AnyRef](snakes: util.List[Snake[T]], forwardVs: util.List[V], reverseVs: util.List[V], pa: Array[T], N: Int, pb: Array[T], M: Int, VForward: V, VReverse: V): Unit =
  CompareImpl(0, snakes, forwardVs, reverseVs, pa, 0, N, pb, 0, M, VForward, VReverse)

  /**
   * Compares two arrays of type <em>T</em> with each other and calculates the shortest edit sequence (SES) as well as
   * the longest common subsequence (LCS) to transfer input <em>a</em> to input <em>b</em>. The SES are the necessary
   * actions required to perform the transformation.
   *
   * @param recursion
   * The number of the current recursive step
   * @param snakes
   * The possible solution paths for transforming object <em>pa </em> to <em>pb</em>
   * @param forwardVs
   * All saved end points in forward direction indexed on <em>d</em>
   * @param reverseVs
   * All saved end points in backward direction indexed on <em>d</em>
   * @param pa
   * Elements of the first object. Usually the original object
   * @param a0
   * The starting position in the array of elements from the first object to compare
   * @param N
   * The number of elements of the first object to compare
   * @param pb
   * Elements of the second object. Usually the current object
   * @param b0
   * The starting position in the array of elements from the second object to compare
   * @param M
   * The number of elements of the second object to compare
   * @param VForward
   * An array of end points for a given k-line in forward direction
   * @param VReverse
   * An array of end points for a given k-line in backward direction
   * @throws Exception
   */
  @throws[Exception]
  private[diff] def CompareImpl[T <: AnyRef](recursion: Int, snakes: util.List[Snake[T]], forwardVs: util.List[V], reverseVs: util.List[V], pa: Array[T], a0: Int, N: Int, pb: Array[T], b0: Int, M: Int, VForward: V, VReverse: V): Unit = {
    if (M == 0 && N > 0) { // add N deletions to SES
      val right = new Snake[T](a0, N, b0, M, true, a0, b0, N, 0, 0)
      if (snakes.size == 0 || !snakes.get(snakes.size - 1).append(right)) snakes.add(right)
    }
    if (N == 0 && M > 0) { // add M insertions to SES
      val down = new Snake[T](a0, N, b0, M, true, a0, b0, 0, M, 0)
      if (snakes.size == 0 || !snakes.get(snakes.size - 1).append(down)) snakes.add(down)
    }
    if (N <= 0 || M <= 0) return
    //calculate middle snake
    val m = LCS.MiddleSnake[T](pa, a0, N, pb, b0, M, VForward, VReverse, forwardVs, reverseVs)
    // Initial setup for recursion
    if (recursion == 0) {
      if (m.getForward != null) m.getForward.setMiddlePoint(true)
      if (m.getReverse != null) m.getReverse.setMiddlePoint(true)
    }
    // check for edge (D = 0 or 1) or middle segment (D > 1)
    if (m.getD > 1) { // solve the rectangles that remain to the top left and bottom right
      // top left .. Compare(A[1..x], x, B[1..y], y)
      val xy = if (m.getForward != null) {
        m.getForward.getStartPoint
      }
      else {
        m.getReverse.getEndPoint
      }
      CompareImpl(recursion + 1, snakes, null, null, pa, a0, xy.X - a0, pb, b0, xy.Y - b0, VForward, VReverse)
      // add middle snake to results
      if (m.getForward != null) if (snakes.size == 0 || !snakes.get(snakes.size - 1).append(m.getForward)) snakes.add(m.getForward)
      if (m.getReverse != null) if (snakes.size == 0 || !snakes.get(snakes.size - 1).append(m.getReverse)) snakes.add(m.getReverse)
      // bottom right .. Compare(A[u+1..N], N-u, B[v+1..M], M-v)
      val uv = if (m.getReverse != null) {
        m.getReverse.getStartPoint
      }
      else {
        m.getForward.getEndPoint
      }
      CompareImpl(recursion + 1, snakes, null, null, pa, uv.X, a0 + N - uv.X, pb, uv.Y, b0 + M - uv.Y, VForward, VReverse)
    }
    else { // we found an edge case. If d == 0 than both segments are identical
      // if d == 1 than there is exactly one insertion or deletion which
      // results in a odd delta and therefore a forward snake
      if (m.getForward != null) { // add d = 0 diagonal to results
        if (m.getForward.XStart > a0) {
          if (m.getForward.XStart - a0 != m.getForward.YStart - b0) throw new Exception("Missed D0 forward")
          val snake = new Snake[T](a0, N, b0, M, true, a0, b0, 0, 0, m.getForward.XStart - a0)
          if (snakes.size == 0 || !snakes.get(snakes.size - 1).append(snake)) snakes.add(snake)
        }
        if (snakes.size == 0 || !snakes.get(snakes.size - 1).append(m.getForward)) snakes.add(m.getForward)
      }
      if (m.getReverse != null) {
        if (snakes.size == 0 || !snakes.get(snakes.size - 1).append(m.getReverse)) snakes.add(m.getReverse)
        // D0
        if (m.getReverse.XStart < a0 + N) {
          if (a0 + N - m.getReverse.XStart != b0 + M - m.getReverse.YStart) throw new Exception("Missed D0 reverse")
          val snake = new Snake[T](a0, N, b0, M, true, m.getReverse.XStart, m.getReverse.YStart, 0, 0, a0 + N - m.getReverse.XStart)
          if (snakes.size == 0 || !snakes.get(snakes.size - 1).append(snake)) snakes.add(snake)
        }
      }
    }
  }
}