package net.wayfarerx.dreamsleeve.diff

import net.wayfarerx.dreamsleeve.model.{Edit, Hash, Value}

/**
 * Performs a linear time and space comparison of two objects by comparing both objects in both directions to find a
 * overlapping path which is called the middle snake.
 *
 * Myers proved that the middle segment is already a part of the solution. Furthermore the middle segment divides the
 * comparison in two sub problems, which further can be compared using this technique.
 *
 * @param from    Usually the older object which should be compared.
 * @param to      Usually the newest object which should be compared.
 * @param builder The hash builder to use for equality.
 * @author wayfarerx
 * @author Roman Vottner
 */
class Differences private(from: Vector[Value], to: Vector[Value])(implicit builder: Hash.Builder) {

  import Differences._
  import Snake.{Forward, Reverse}

  /** An array of end points for a given k-line in forward direction. */
  private val forwardPath = new Path(from.length, to.length, true)
  /** An array of end points for a given k-line in reverse direction. */
  private val reversePath = new Path(from.length, to.length, false)

  /**
   * Calculates the shortest sequence of edits that will transform the first sequence into the second.
   *
   * @return The shortest sequence of edits that will transform the first sequence into the second.
   */
  private def edits(): Vector[Edit] = {
    diff(0, 0, from.length, 0, to.length, Snakes()).snakes flatMap { snake =>
      var edits = Vector[Edit]()
      if (snake.forward) {
        if (snake.deleted > 0) edits :+= Edit.Remove(from.slice(snake.xStart, snake.xMid).map(_.hash))
        if (snake.inserted > 0) edits :+= Edit.Insert(to.slice(snake.yStart, snake.yMid))
        if (snake.diagonals > 0) edits :+= Edit.Copy(from.slice(snake.xMid, snake.xEnd).map(_.hash))
      } else {
        if (snake.diagonals > 0) edits :+= Edit.Copy(from.slice(snake.xEnd, snake.xMid).map(_.hash))
        if (snake.inserted > 0) edits :+= Edit.Insert(to.slice(snake.yMid, snake.yStart))
        if (snake.deleted > 0) edits :+= Edit.Remove(from.slice(snake.xMid, snake.xStart).map(_.hash))
      }
      edits
    }
  }

  /**
   * Compares two vectors with each other and calculates the shortest edit sequence as well as the longest common
   * sub-sequence to transfer input between the underlying sets of data.
   *
   * @param depth     The number of the current recursive step.
   * @param fromStart The starting position in the vector of elements from the first object to compare.
   * @param fromCount The number of elements of the first object to compare.
   * @param toStart   The starting position in the vector of elements from the second object to compare.
   * @param toCount   The number of elements of the second object to compare.
   * @param results   The possible solution paths for transforming the underlying data.
   * @return The possible solution paths for transforming the underlying data.
   */
  private def diff(depth: Int, fromStart: Int, fromCount: Int, toStart: Int, toCount: Int, results: Snakes): Snakes = {
    var snakes = results
    if (toCount == 0 && fromCount > 0) // add fromCount deletions to SES
      snakes :+= Forward(fromStart, fromCount, toStart, toCount, fromStart, toStart, fromCount, 0, 0)
    else if (fromCount == 0 && toCount > 0) // add toCount insertions to SES
      snakes :+= Forward(fromStart, fromCount, toStart, toCount, fromStart, toStart, 0, toCount, 0)
    else if (fromCount > 0 && toCount > 0) {
      //calculate middle snake
      val (diffs, snake) = middle(fromStart, fromCount, toStart, toCount)
      // check for edge (diffs = 0 or 1) or middle segment (diffs > 1)
      if (diffs > 1) { // solve the rectangles that remain to the top left and bottom right
        // top left .. Compare(from[1..x], x, to[1..y], y)
        val (x, y) = if (snake.forward) (snake.xStart, snake.yStart) else (snake.xEnd, snake.yEnd)
        snakes = diff(depth + 1, fromStart, x - fromStart, toStart, y - toStart, snakes)
        // add middle snake to results
        snakes :+= snake
        // bottom right .. Compare(from[u+1..fromCount], fromCount-u, to[v+1..toCount], toCount-v)
        val (u, v) = if (snake.forward) (snake.xEnd, snake.yEnd) else (snake.xStart, snake.yStart)
        snakes = diff(depth + 1, u, fromStart + fromCount - u, v, toStart + toCount - v, snakes)
      } else { // we found an edge case. If diffs == 0 than both segments are identical
        // if diffs == 1 than there is exactly one insertion or deletion which
        // results in a odd delta and therefore a forward snake
        if (snake.forward) { // add diffs = 0 diagonal to results
          if (snake.xStart > fromStart) {
            if (snake.xStart - fromStart != snake.yStart - toStart)
              sys.error("Missed diffs == 0 forward")
            snakes :+= Forward(
              fromStart,
              fromCount,
              toStart,
              toCount,
              fromStart,
              toStart,
              0,
              0,
              snake.xStart - fromStart)
          }
          snakes :+= snake
        } else {
          snakes :+= snake
          if (snake.xStart < fromStart + fromCount) {
            if (fromStart + fromCount - snake.xStart != toStart + toCount - snake.yStart)
              sys.error("Missed diffs == 0 reverse")
            snakes :+= Forward(
              fromStart,
              fromCount,
              toStart,
              toCount,
              snake.xStart,
              snake.yStart,
              0,
              0,
              fromStart + fromCount - snake.xStart)
          }
        }
      }
    }
    snakes
  }

  /**
   * Calculates the middle snake segment by comparing object the two underlying tables in both directions at the same
   * time. The overlap of both comparisons is the so called middle snake which is already a part of the solution as
   * proven by Myers.
   *
   * @param fromStart The starting position in the array of elements from the first object to compare.
   * @param fromCount The number of elements of the first object to compare.
   * @param toStart   The starting position in the array of elements from the second object to compare.
   * @param toCount   The number of elements of the second object to compare.
   * @return The first segment found by both comparison directions which is also called the middle snake.
   */
  private def middle(fromStart: Int, fromCount: Int, toStart: Int, toCount: Int): (Int, Snake) = {
    // we only need to find a middle snake with diffs which is half of the diffs of the forward and reverse algorithms.
    val max = (fromCount + toCount + 1) / 2
    val delta = fromCount - toCount
    forwardPath.reset(fromCount, toCount)
    reversePath.reset(fromCount, toCount)
    // Each difference - a horizontal deletion or a vertical insertion - is a move from on k line to its neighbor. As
    // delta is the difference between the centers of the forward and reverse algorithms, we know which values of diffs
    // we need to check for a middle snake.
    val evenDelta = (delta % 2) == 0
    var diffs = 0
    while (diffs <= max) {
      // forward checks against reverse diffs-1
      {
        // An important observation for the implementation is that end points for even diffs are on even k-lines only
        // and vice-versa. That's why k+=2
        var k = -diffs
        while (k <= diffs) { // calculate the farthest reaching forward path on line k
          val down = k == -(diffs) || (k != diffs && forwardPath.apply(k - 1) < forwardPath.apply(k + 1))
          // to get to a line k, we either must move down (k+1) or right (k-1)
          val xStart = if (down) forwardPath.apply(k + 1) else forwardPath.apply(k - 1)
          // y can easily calculated by subtracting k from x --> y = x - k
          val yStart = xStart - (if (down) k + 1 else k - 1)
          var xEnd = if (down) xStart else xStart + 1
          var yEnd = xEnd - k
          var snake = 0
          while (xEnd < fromCount && yEnd < toCount && equal(from(xEnd + fromStart), to(yEnd + toStart))) {
            xEnd += 1
            yEnd += 1
            snake += 1
          }
          forwardPath.update(k, xEnd)
          // for odd delta, we must look for overlap of forward paths with differences diffs and reverse paths with
          // differences diffs-1 if Δ is odd and k ϵ [ Δ - ( diffs - 1 ), Δ + ( diffs - 1 ) ]
          if (!(evenDelta || k < delta - (diffs - 1) || k > delta + (diffs - 1))) {
            // check if the path overlaps the farthest reaching reverse ( diffs - 1 )-path in diagonal k
            if (forwardPath.apply(k) >= reversePath.apply(k)) {
              // we found a middle snake and the shortest edit script of length 2D -1
              return ((2 * diffs) - 1,
                Forward(fromStart, fromCount, toStart, toCount, xStart + fromStart, yStart + toStart, down, snake))
            }
          }
          k += 2
        }
      }
      // reverse checks against forward diffs
      var k = -diffs + delta
      while (k <= diffs + delta) { // calculate the farthest reaching reverse path on line k
        val up = k == diffs + delta || (k != -(diffs) + delta && reversePath.apply(k - 1) < reversePath.apply(k + 1))
        // to get to a line k, we either must move up (k-1) or left (k+1)
        val xStart = if (up) reversePath.apply(k - 1) else reversePath.apply(k + 1)
        val yStart = xStart - (if (up) k - 1 else k + 1)
        var xEnd = if (up) xStart else xStart - 1
        var yEnd = xEnd - k
        var snake = 0
        while (xEnd > 0 && yEnd > 0 && equal(from(xEnd + fromStart - 1), to(yEnd + toStart - 1))) {
          xEnd -= 1
          yEnd -= 1
          snake += 1
        }
        reversePath.update(k, xEnd)
        // remember: our k is actually k + Δ if Δ is even and k + Δ ϵ [ -diffs, diffs ]
        if (!(!evenDelta || k < -diffs || k > diffs)) {
          // check if the path overlaps the farthest reaching forward diffs-path in diagonal k + Δ
          if (reversePath.apply(k) <= forwardPath.apply(k)) {
            // (SES) of length 2D
            return (2 * diffs,
              Reverse(fromStart, fromCount, toStart, toCount, xStart + fromStart, yStart + toStart, up, snake))
          }
        }
        k += 2
      }
      diffs += 1
    }
    sys.error("No middle snake")
  }

  /**
   * Compares two values for equality.
   *
   * @param from The original value to compare.
   * @param to   The resulting value to compare.
   * @return True if the specified values are equal.
   */
  private def equal(from: Value, to: Value) =
    from.hash == to.hash && from == to

}

/**
 * Performs a linear time and space comparison of two objects by comparing both objects in both directions to find a
 * overlapping path which is called the middle snake.
 *
 * Myers proved that the middle segment is already a part of the solution. Furthermore the middle segment divides the
 * comparison in two sub problems, which further can be compared using this technique.
 *
 * @author wayfarerx
 * @author Roman Vottner
 */
object Differences {

  /**
   * Calculates the shortest sequence of edits that will transform the first sequence into the second.
   *
   * @param from    The original sequence.
   * @param to      The resulting sequence.
   * @param builder The hash builder to use for equality.
   * @return The shortest sequence of edits that will transform the first sequence into the second.
   */
  def apply(from: Vector[Value], to: Vector[Value])(implicit builder: Hash.Builder): Vector[Edit] =
    new Differences(from, to).edits()

  /**
   * A sequence of snakes that moves through the comparison graph.
   *
   * @param snakes The snakes that exist in this sequence.
   */
  private case class Snakes(snakes: Vector[Snake] = Vector()) {

    /**
     * Appends a snake to this sequence.
     *
     * @param snake The snake to append.
     * @return This sequence of snakes with the specified snake appended.
     */
    def :+(snake: Snake): Snakes =
      if (snakes.isEmpty) Snakes(snakes :+ snake)
      else snakes.last.append(snake) match {
        case Some(snake) => Snakes(snakes.init :+ snake)
        case None => Snakes(snakes :+ snake)
      }

  }

}