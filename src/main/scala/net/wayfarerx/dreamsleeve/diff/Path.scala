package net.wayfarerx.dreamsleeve.diff

/**
 * This class is a helper class to store the actual x-positions of end-points on a k-line.
 *
 * It further provides a method to calculate the y-position for end-points based on the x-position and the k-line the
 * end point is lying on.
 *
 * @param from    The length of the first object which gets compared to the second.
 * @param to      The length of the second object which gets compared to the first.
 * @param forward True if this path moves from (0, 0) to (x, y), false otherwise.
 * @author wayfarerx
 * @author Roman Vottner
 */
private[diff] class Path(from: Int, to: Int, forward: Boolean) {

  /** The maximum number of end points to store. */
  private val max = Math.max(1, (from + to) / 2 + 1)
  /** Stores the actual x-positions. */
  private val array = new Array[Int](2 * max + 1)
  /** As the length of A and B can be different, the k lines of the forward and reverse algorithms can be different. */
  private var delta = 0

  /**
   * Resets the k-line based on the comparison direction.
   *
   * @param from The length of the first object to compare.
   * @param to   The length of the second object to compare.
   */
  def reset(from: Int, to: Int): Unit =
    if (forward) update(1, 0) // stub for forward
    else {
      delta = from - to
      update(from - to - 1, from) // stub for backward
    }

  /**
   * Returns the x-position for an end point for a given k-line
   *
   * @param k The k-line to recall the x-position for
   * @return The x-position of an end point
   */
  def apply(k: Int): Int =
    array(k - delta + max)

  /**
   * Stores the x-position of an end point for a given k-line.
   *
   * @param k     The k-line to store the position for
   * @param value The x-position of the end point
   */
  def update(k: Int, value: Int): Unit =
    array(k - delta + max) = value

}
