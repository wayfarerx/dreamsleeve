package net.wayfarerx.dreamsleeve.diff

/**
 * Utility class that hold both directional calculations for the segment the snake is used for.
 *
 * @param D       The number of differences for both segment calculations
 * @param Forward The segment calculated in a forward direction
 * @param Reverse The segment calculated in a backward direction
 * @tparam T The type of the element the snakes will hold
 * @author Roman Vottner
 */
class SnakePair[T](var D: Int, var Forward: Snake[T], var Reverse: Snake[T]) {
  /**
   * Sets the number of differences for both calculation directions.
   *
   * @param d The number of differences for both calculation directions
   */
  def setD(d: Int): Unit = this.D = d

  /**
   * Returns the number of differences for both calculation directions.
   * <p>
   * A value of 0 indicates that compared elements from the first and the second object are equal. A value of 1
   * indicates either an insertion from the second object or a deletion from the first object.
   * <p>
   * Moreover, a value of 0 must be a reverse segment, while a value of 1 results from a forward segment.
   *
   * @return The number of differences for both calculation directions
   */
  def getD = this.D

  /**
   * Sets the new segment calculated in a forward direction.
   *
   * @param forward The segment calculated in forward direction
   */
  def setForward(forward: Snake[T]): Unit = this.Forward = forward

  /**
   * Returns the segment which was calculated in forward direction.
   *
   * @return The segment calculated in forward direction
   */
  def getForward = this.Forward

  /**
   * Sets the new segment calculated in a backward direction.
   *
   * @param reverse The segment calculated in backward direction
   */
  def setReverse(reverse: Snake[T]): Unit = this.Reverse = reverse

  /**
   * Returns the segment which was calculated in backward direction.
   *
   * @return The segment calculated in backward direction
   */
  def getReverse = this.Reverse
}