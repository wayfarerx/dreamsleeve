package net.wayfarerx.dreamsleeve.diff

/**
 * This class is a helper class to store the actual x-positions of end-points on a k-line.
 *
 * It further provides a method to calculate the y-position for end-points based on the x-position and the k-line the
 * end point is lying on.
 *
 * @param forward Comparison direction flag
 * @param max     The maximum number of end points to store
 * @param delta   As the length of A and B can be different, the k lines of the forward and reverse algorithms can be
 *                different. It is useful to isolate this as a variable
 * @param array   Stores the actual x-position
 * @author Roman Vottner
 */
private[diff] class V private(
  forward: Boolean,
  max: Int,
  private var delta: Int,
  private var array: Array[Int]
) {

  /**
   * Returns the x-position for an end point for a given k-line
   *
   * @param k The k-line to recall the x-position for
   * @return The x-position of an end point
   */
  def getK(k: Int): Int =
    array(k - delta + max)

  /**
   * Stores the x-position of an end point for a given k-line.
   *
   * @param k     The k-line to store the position for
   * @param value The x-position of the end point
   */
  def setK(k: Int, value: Int): Unit =
    array(k - delta + max) = value

  /**
   * Initializes the k-line based on the comparison direction.
   *
   * @param n The length of the first object to compare
   * @param m The length of the second object to compare
   */
  def InitStub(n: Int, m: Int): Unit =
    if (forward) setK(1, 0) // stub for forward
    else {
      delta = n - m
      setK(n - m - 1, n) // stub for backward
    }

  /**
   * Creates a new deep copy of this object.
   *
   * @param d       Number of differences for the same trace
   * @param forward The comparison direction; True if forward, false otherwise
   * @param delta   Keeps track of the differences between the first and the second object to compare as they may differ in
   *                length
   * @return The deep copy of this object
   * @throws Exception If d > the maximum number of end points to store
   */
  def CreateCopy(d: Int, forward: Boolean, delta: Int): V = {
    val _max = Math.max(1, d)
    if (_max > max) sys.error("V.CreateCopy")
    val _delta = if (forward) 0 else delta
    val _array = new Array[Int](2 * _max + 1)
    System.arraycopy(array, (max - delta) - (_max - _delta), _array, 0, _array.length)
    new V(forward, _max, _delta, _array)
  }

}

private[diff] object V {

  /**
   * Initializes a new instance of this helper class.
   *
   * @param n       The length of the first object which gets compared to the second
   * @param m       The length of the second object which gets compared to the first
   * @param forward The comparison direction; True if forward, false otherwise
   */
  def apply(n: Int, m: Int, forward: Boolean): V = {
    val max = Math.max(1, (n + m) / 2 + 1)
    val v = new V(forward, max, 0, new Array[Int](2 * max + 1))
    v.InitStub(n, m)
    v
  }

}