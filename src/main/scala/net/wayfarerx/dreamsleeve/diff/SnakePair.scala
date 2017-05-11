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
case class SnakePair[T](
  D: Int,
  Forward: Snake,
  Reverse: Snake
) {

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
  def getD = D

  /**
   * Returns the segment which was calculated in forward direction.
   *
   * @return The segment calculated in forward direction
   */
  def getForward = Forward

  /**
   * Returns the segment which was calculated in backward direction.
   *
   * @return The segment calculated in backward direction
   */
  def getReverse = Reverse
}