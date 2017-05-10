package net.wayfarerx.dreamsleeve.diff

/**
 * This class is a helper class to store the actual x-positions of end-points on a k-line.
 * <p>
 * It further provides a method to calculate the y-position for end-points based on the x-position and the k-line the
 * end point is lying on.
 *
 * @param IsForward Comparison direction flag
 * @param N         Length of the first input string
 * @param M         Length of the second input string
 * @param max       The maximum number of end points to store
 * @param delta     As the length of A and B can be different, the k lines of the forward and reverse algorithms can be different. It
 *                  is useful to isolate this as a variable
 * @param array     Stores the actual x-position
 * @author Roman Vottner
 */
private[diff] class V private(
                               private var IsForward: Boolean = false,
                               private var N: Int = 0,
                               private var M: Int = 0,
                               private var max: Int = 0,
                               private var delta: Int = 0,
                               private var array: Array[Int] = null
                             ) {

  /**
   * Stores the x-position of an end point for a given k-line.
   *
   * @param k     The k-line to store the position for
   * @param value The x-position of the end point
   */
  def setK(k: Int, value: Int): Unit = this.array(k - this.delta + this.max) = value

  /**
   * Returns the x-position for an end point for a given k-line
   *
   * @param k The k-line to recall the x-position for
   * @return The x-position of an end point
   */
  def getK(k: Int): Int = array(k - this.delta + this.max)

  /**
   * Calculates the y-position of an end point based on the x-position and the k-line.
   *
   * @param k The k-line the end point is on
   * @return The y-position of the end point
   */
  def Y(k: Int): Int = this.getK(k) - k

  /**
   * Returns the comparison direction.
   *
   * @return True if the comparison direction is forward, false otherwise
   */
  def isForward = this.IsForward

  /**
   * Returns the length of the second object which gets compared to the first.
   *
   * @return The length of the second object
   */
  def getM = this.M

  /**
   * Returns the length of the first object which gets compared to the second.
   *
   * @return The length of the first object
   */
  def getN = this.N

  /**
   * Initializes the k-line based on the comparison direction.
   *
   * @param n The length of the first object to compare
   * @param m The length of the second object to compare
   */
  def InitStub(n: Int, m: Int): Unit =
    if (this.IsForward) this.setK(1, 0) // stub for forward
    else {
      this.delta = n - m
      this.setK(n - m - 1, n) // stub for backward
    }

  /**
   * Creates a new deep copy of this object.
   *
   * @param d         Number of differences for the same trace
   * @param isForward The comparison direction; True if forward, false otherwise
   * @param delta     Keeps track of the differences between the first and the second object to compare as they may differ in
   *                  length
   * @return The deep copy of this object
   * @throws Exception If d > the maximum number of end points to store
   */
  @throws[Exception]
  def CreateCopy(d: Int, isForward: Boolean, delta: Int): V = {
    val dd = if (d == 0) d + 1 else d
    val o = new V
    o.IsForward = isForward
    o.max = dd
    if (!isForward) o.delta = delta
    o.array = new Array[Int](2 * dd + 1)
    if (dd <= this.max) System.arraycopy(this.array, (this.max - this.delta) - (o.max - o.delta), o.array, 0, o.array.length)
    else throw new Exception("V.CreateCopy")
    o
  }

}

object V {

  /**
   * Initializes a new instance of this helper class.
   *
   * @param n       The length of the first object which gets compared to the second
   * @param m       The length of the second object which gets compared to the first
   * @param forward The comparison direction; True if forward, false otherwise
   * @param linear  True if a linear comparison should be used for comparing two objects or the greedy method (false)
   */
  def apply(n: Int, m: Int, forward: Boolean, linear: Boolean): V = {
    val max = Math.max(1, if (linear) (n + m) / 2 + 1 else n + m)
    val v = new V(forward, n, m, max, array = new Array[Int](2 * max + 1))
    v.InitStub(n, m)
    v
  }

}