package net.wayfarerx.dreamsleeve
package diff

import java.util

import scala.collection.JavaConverters._

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
    Compare(from.toArray, to.toArray) flatMap { snake =>
      var edits = Vector[model.Edit]()
      if (snake.forward) {
        if (snake.deleted > 0) edits :+= model.Edit.Remove(from.slice(snake.xStart, snake.xMid).map(_.hash()))
        if (snake.inserted > 0) edits :+= model.Edit.Insert(to.slice(snake.yStart, snake.yMid))
        if (snake.diagonal > 0) edits :+= model.Edit.Copy(from.slice(snake.xMid, snake.xEnd).map(_.hash()))
      } else {
        if (snake.diagonal > 0) edits :+= model.Edit.Copy(from.slice(snake.xEnd, snake.xMid).map(_.hash()))
        if (snake.inserted > 0) edits :+= model.Edit.Insert(to.slice(snake.yMid, snake.yStart))
        if (snake.deleted > 0) edits :+= model.Edit.Remove(from.slice(snake.xMid, snake.xStart).map(_.hash()))
      }
      edits
    }
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
   * @return The snake that lead from input <em>aa</em> to input <em>ab</em>
   * @throws Exception
   */
  @throws[Exception]
  def Compare[T <: AnyRef](aa: Array[T], ab: Array[T]): Vector[Snake] = {
    val VForward = V(aa.length, ab.length, true)
    val VReverse = V(aa.length, ab.length, false)
    val snakes = new util.ArrayList[Snake]
    val forwardVs = new util.ArrayList[V]
    val reverseVs = new util.ArrayList[V]
    CompareSnakes(snakes, forwardVs, reverseVs, aa, aa.length, ab, ab.length, VForward, VReverse)
    snakes.asScala.toVector
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
  private[diff] def CompareSnakes[T <: AnyRef](snakes: util.List[Snake], forwardVs: util.List[V], reverseVs: util.List[V], pa: Array[T], N: Int, pb: Array[T], M: Int, VForward: V, VReverse: V): Unit =
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
  private[diff] def CompareImpl[T <: AnyRef](recursion: Int, snakes: util.List[Snake], forwardVs: util.List[V], reverseVs: util.List[V], pa: Array[T], a0: Int, N: Int, pb: Array[T], b0: Int, M: Int, VForward: V, VReverse: V): Unit = {
    if (M == 0 && N > 0) { // add N deletions to SES
      val right = Snake.Forward(a0, N, b0, M, a0, b0, N, 0, 0)
      if (snakes.size == 0) snakes.add(right)
      else snakes.get(snakes.size - 1).append(right) match {
        case Some(newSnake) =>
          snakes.remove(snakes.size - 1)
          snakes.add(newSnake)
        case None =>
          snakes.add(right)
      }
    }
    if (N == 0 && M > 0) { // add M insertions to SES
      val down = Snake.Forward(a0, N, b0, M, a0, b0, 0, M, 0)
      if (snakes.size == 0) snakes.add(down)
      else snakes.get(snakes.size - 1).append(down) match {
        case Some(newSnake) =>
          snakes.remove(snakes.size - 1)
          snakes.add(newSnake)
        case None =>
          snakes.add(down)
      }
    }
    if (N <= 0 || M <= 0) return
    //calculate middle snake
    val (mDiffs, mSnake) = LCS.MiddleSnake[T](pa, a0, N, pb, b0, M, VForward, VReverse, forwardVs, reverseVs)
    // check for edge (D = 0 or 1) or middle segment (D > 1)
    if (mDiffs > 1) { // solve the rectangles that remain to the top left and bottom right
      // top left .. Compare(A[1..x], x, B[1..y], y)
      val xy = if (mSnake.forward) (mSnake.xStart, mSnake.yStart) else (mSnake.xEnd, mSnake.yEnd)
      CompareImpl(recursion + 1, snakes, null, null, pa, a0, xy._1 - a0, pb, b0, xy._2 - b0, VForward, VReverse)
      // add middle snake to results
      if (snakes.size == 0) snakes.add(mSnake)
      else snakes.get(snakes.size - 1).append(mSnake) match {
        case Some(newSnake) =>
          snakes.remove(snakes.size - 1)
          snakes.add(newSnake)
        case None =>
          snakes.add(mSnake)
      }
      // bottom right .. Compare(A[u+1..N], N-u, B[v+1..M], M-v)
      val uv = if (mSnake.forward) (mSnake.xEnd, mSnake.yEnd) else (mSnake.xStart, mSnake.yStart)
      CompareImpl(recursion + 1, snakes, null, null, pa, uv._1, a0 + N - uv._1, pb, uv._2, b0 + M - uv._2, VForward, VReverse)
    }
    else { // we found an edge case. If d == 0 than both segments are identical
      // if d == 1 than there is exactly one insertion or deletion which
      // results in a odd delta and therefore a forward snake
      if (mSnake.forward) { // add d = 0 diagonal to results
        if (mSnake.xStart > a0) {
          if (mSnake.xStart - a0 != mSnake.yStart - b0) sys.error("Missed D0 forward")
          val snake = Snake.Forward(a0, N, b0, M, a0, b0, 0, 0, mSnake.xStart - a0)
          if (snakes.size == 0) snakes.add(snake)
          else snakes.get(snakes.size - 1).append(snake) match {
            case Some(newSnake) =>
              snakes.remove(snakes.size - 1)
              snakes.add(newSnake)
            case None =>
              snakes.add(snake)
          }
        }
        if (snakes.size == 0) snakes.add(mSnake)
        else snakes.get(snakes.size - 1).append(mSnake) match {
          case Some(newSnake) =>
            snakes.remove(snakes.size - 1)
            snakes.add(newSnake)
          case None =>
            snakes.add(mSnake)
        }
      } else {
        if (snakes.size == 0) snakes.add(mSnake)
        else snakes.get(snakes.size - 1).append(mSnake) match {
          case Some(newSnake) =>
            snakes.remove(snakes.size - 1)
            snakes.add(newSnake)
          case None =>
            snakes.add(mSnake)
        }
        // D0
        if (mSnake.xStart < a0 + N) {
          if (a0 + N - mSnake.xStart != b0 + M - mSnake.yStart) throw new Exception("Missed D0 reverse")
          val snake = Snake.Forward(a0, N, b0, M, mSnake.xStart, mSnake.yStart, 0, 0, a0 + N - mSnake.xStart)
          if (snakes.size == 0) snakes.add(snake)
          else snakes.get(snakes.size - 1).append(snake) match {
            case Some(newSnake) =>
              snakes.remove(snakes.size - 1)
              snakes.add(newSnake)
            case None =>
              snakes.add(snake)
          }
        }
      }
    }
  }
}