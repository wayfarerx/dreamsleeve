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
    val (mDiffs, mSnake) = MiddleSnake[T](pa, a0, N, pb, b0, M, VForward, VReverse, forwardVs, reverseVs)
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

  /**
   * Calculates the middle snake segment by comparing object <em>pa</em> with <em>pb</em> in both directions at
   * the same time for consecutive <em>D</em>'s. The overlap of both comparisons is the so called middle snake which
   * is already a part of the solution as proven by Myers.
   *
   * @param pa        Usually the older object which should be compared
   * @param a0        The starting position in the array of elements from the first object to compare
   * @param N         The number of elements of the first object to compare
   * @param pb        Usually the newest object to be compared with <em>pa</em>
   * @param b0        The starting position in the array of elements from the second object to compare
   * @param M         The number of elements of the second object to compare
   * @param VForward  An array of end points for a given k-line for the forward comparison
   * @param VReverse  An array of end points for a given k-line for the backward comparison
   * @param forwardVs All saved end points indexed on <em>d</em> for the forward comparison
   * @param reverseVs All saved end points indexed on <em>d</em> for the backward comparison
   * @return The first segment found by both comparison directions which is also called the middle snake
   * @throws Exception If no middle snake could be found
   */
  private def MiddleSnake[T](pa: Array[T], a0: Int, N: Int, pb: Array[T], b0: Int, M: Int, VForward: V, VReverse: V, forwardVs: util.List[V], reverseVs: util.List[V]): (Int, Snake) = {
    // we only need to find a middle snake with a d which is half of the
    // d of the forward and reverse algorithms.
    val MAX = (N + M + 1) / 2
    val DELTA = N - M
    VForward.InitStub(N, M)
    VReverse.InitStub(N, M)
    // Each difference - a horizontal deletion or a vertical insertion - is
    // a move from on k line to its neighbor. As delta is the difference
    // between the centers of the forward and reverse algorithms, we know
    // which values of d we need to check for a middle snake.
    val DeltaIsEven = (DELTA % 2) == 0
    var d = 0
    while (d <= MAX) { // forward
      // checks against reverse D-1
      try { // An important observation for the implementation is that end
        // points for even d are on even k-lines only and vice-versa.
        // That's why k+=2
        var k = -d
        while (k <= d) { // calculate the farthest reaching forward path on line k
          val down = k == -(d) || (k != d && VForward.getK(k - 1) < VForward.getK(k + 1))
          // to get to a line k, we either must move down (k+1) or
          // right (k-1)
          val xStart = if (down) VForward.getK(k + 1)
          else VForward.getK(k - 1)
          // y can easily calculated by subtracting k from
          // x --> y = x - k
          val yStart = xStart - (if (down) k + 1 else k - 1)
          var xEnd = if (down) xStart
          else xStart + 1
          var yEnd = xEnd - k
          var snake = 0
          while (xEnd < N && yEnd < M && pa(xEnd + a0) == pb(yEnd + b0)) {
            xEnd += 1
            yEnd += 1
            snake += 1
          }
          VForward.setK(k, xEnd)
          // for odd delta, we must look for overlap of forward paths
          // with differences d and reverse paths with differences d-1
          // if Δ is odd and k ϵ [ Δ - ( D - 1 ), Δ + ( D - 1 ) ]
          if (!(DeltaIsEven || k < DELTA - (d - 1) || k > DELTA + (d - 1))) {
            // check if the path overlaps the farthest reaching reverse
            // ( D - 1 )-path in diagonal k
            if (VForward.getK(k) >= VReverse.getK(k)) {
              // overlap :)
              val forward = Snake.Forward(a0, N, b0, M, xStart + a0, yStart + b0, down, snake)
              // we found a middle snake and the shortest edit script
              // (SES) of length 2D -1
              return ((2 * d) - 1, forward)
            }
          }
          k += 2
        }
      } finally if (forwardVs != null) forwardVs.add(VForward.CreateCopy(d, true, 0))
      // backward
      // checks against forward D
      try {
        var k = -d + DELTA
        while (k <= d + DELTA) { // calculate the farthest reaching reverse path on line k
          val up = k == d + DELTA || (k != -(d) + DELTA && VReverse.getK(k - 1) < VReverse.getK(k + 1))
          // to get to a line k, we either must move up (k-1) or left
          // (k+1)
          val xStart = if (up) VReverse.getK(k - 1)
          else VReverse.getK(k + 1)
          val yStart = xStart - (if (up) k - 1
          else k + 1)
          var xEnd = if (up) xStart
          else xStart - 1
          var yEnd = xEnd - k
          var snake = 0
          while (xEnd > 0 && yEnd > 0 && pa(xEnd + a0 - 1) == pb(yEnd + b0 - 1)) {
            xEnd -= 1
            yEnd -= 1
            snake += 1
          }
          VReverse.setK(k, xEnd)
          // remember: our k is actually k + Δ
          // if Δ is even and k + Δ ϵ [ -D, D ]
          if (!(!DeltaIsEven || k < -d || k > d)) {
            // check if the path overlaps the farthest reaching forward
            // D-path in diagonal k + Δ
            if (VReverse.getK(k) <= VForward.getK(k)) {
              val reverse = Snake.Reverse(a0, N, b0, M, xStart + a0, yStart + b0, up, snake)
              // (SES) of length 2D
              return (2 * d, reverse)
            }
          }
          k += 2
        }
      } finally if (reverseVs != null) reverseVs.add(VReverse.CreateCopy(d, false, DELTA))
      d += 1
    }
    throw new Exception("No middle snake")
  }

}