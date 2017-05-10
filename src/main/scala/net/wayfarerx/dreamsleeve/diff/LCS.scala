package net.wayfarerx.dreamsleeve.diff

import java.util

/**
 * <p> Provides functions to calculate the longest common subsequence (LCS) for forward, backward and in-between
 * estimations. </p>
 *
 * @author Roman Vottner
 */
private[diff] object LCS {
  /**
   * Calculates the longest common subsequence (LCS) in a forward manner for two objects <em>pa</em> and
   * <em>pb</em>.
   *
   * @param pa Usually the older object which should be compared
   * @param N  The number of elements of the first object to compare
   * @param pb Usually the newest object to be compared with <em>pa</em>
   * @param M  The number of elements of the second object to compare
   * @param V  An array of end points for a given k-line
   * @param d  The number of differences for the same trace
   * @return The segment found by forward comparison
   */
  private[diff] def Forward[T](pa: Array[T], N: Int, pb: Array[T], M: Int, V: V, d: Int): Snake[T] = {
    // An important observation for the implementation is that end points
    // for even d are on even k-lines only and vice-versa. That's why k+=2
    var k = -d
    while (k <= d) { // are we on the down track?
      val down = k == -(d) || (k != d && V.getK(k - 1) < V.getK(k + 1))
      // to get to a line k, we either must move down (k+1) or right (k-1)
      val xStart = if (down) V.getK(k + 1)
      else V.getK(k - 1)
      // y can easily calculated by subtracting k from x --> y = x - k
      val yStart = xStart - (if (down) k + 1
      else k - 1)
      // calculate end points
      var xEnd = if (down) xStart
      else xStart + 1
      var yEnd = xEnd - k
      var snake = 0
      // follow diagonals
      while (xEnd < N && yEnd < M && pa(xEnd) == pb(yEnd)) {
        xEnd += 1
        yEnd += 1
        snake += 1
      }
      // save end points
      V.setK(k, xEnd)
      // check for solution
      if (xEnd >= N && yEnd >= M) { // solution has been found
        return new Snake[T](0, N, 0, M, true, xStart, yStart, down, snake)
      }
      k += 2
    }
    null
  }

  /**
   * Calculates the longest common subsequence (LCS) in a backward manner for two objects <em>pa</em> and
   * <em>pb</em>.
   *
   * @param pa Usually the older object which should be compared
   * @param N  The number of elements of the first object to compare
   * @param pb Usually the newest object to be compared with <em>pa</em>
   * @param M  The number of elements of the second object to compare
   * @param V  An array of end points for a given k-line
   * @param d  The number of differences for the same trace
   * @return The segment found by reverse comparison
   */
  private[diff] def Reverse[T](pa: Array[T], N: Int, pb: Array[T], M: Int, V: V, d: Int): Snake[T] = {
    // As the length of sequences pa and pb can be different, the k lines of
    // the forward and reverse algorithms can be different. It is useful to
    // isolate this difference as a variable.
    val DELTA = N - M
    var k = -d + DELTA
    while (k <= d + DELTA) { // are we on the down up-track or on the left one?
      val up = k == d + DELTA || (k != -(d) + DELTA && V.getK(k - 1) < V.getK(k + 1))
      // to get to a line k, we either must move up (k-1) or left (k+1)
      val xStart = if (up) V.getK(k - 1)
      else V.getK(k + 1)
      val yStart = xStart - (if (up) k - 1
      else k + 1)
      var xEnd = if (up) xStart
      else xStart - 1
      var yEnd = xEnd - k
      var snake = 0
      while (xEnd > 0 && yEnd > 0 && pa(xEnd - 1) == pb(yEnd - 1)) {
        xEnd -= 1
        yEnd -= 1
        snake += 1
      }
      V.setK(k, xEnd)
      if (xEnd <= 0 && yEnd <= 0) return new Snake[T](0, N, 0, M, false, xStart, yStart, up, snake)
      k += 2
    }
    null
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
  @throws[Exception]
  private[diff] def MiddleSnake[T](pa: Array[T], a0: Int, N: Int, pb: Array[T], b0: Int, M: Int, VForward: V, VReverse: V, forwardVs: util.List[V], reverseVs: util.List[V]): SnakePair[T] = { // we only need to find a middle snake with a d which is half of the
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
              val forward = new Snake[T](a0, N, b0, M, true, xStart + a0, yStart + b0, down, snake)
              forward.setD(d)
              // we found a middle snake and the shortest edit script
              // (SES) of length 2D -1
              return new SnakePair[T]((2 * d) - 1, forward, null)
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
              val reverse = new Snake[T](a0, N, b0, M, false, xStart + a0, yStart + b0, up, snake)
              reverse.setD(d)
              // (SES) of length 2D
              return new SnakePair[T](2 * d, null, reverse)
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