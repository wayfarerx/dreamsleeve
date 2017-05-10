package net.wayfarerx.dreamsleeve.diff;

/**
 * A snake is a segment along a path which converts an object A to object B by either eliminating elements from object A
 * or inserting elements from object B.
 *
 * @param XStart         The x-position of a starting point
 * @param YStart         The y-position of a starting point
 * @param ADeleted       Defines the number of deleted elements from the first object to match the second object
 * @param BInserted      Defines the number of inserted elements from the second object to match the first object
 * @param DiagonalLength Defines the number of equal elements in both objects
 * @param IsForward      Defines the comparison direction of both objects
 * @param DELTA          The difference in length between the first and second object to compare. This value is used as an offset between
 *                       the forward k lines to the reverse ones
 * @param IsMiddle       Defines if this snake is a middle segment
 * @param D              A value of 0 or 1 indicate an edge, where 0 means both objects are equal while 1 means there is either one
 *                       insertion or one deletion. A value of greater than needs to be checked in both directions
 * @author Roman Vottner
 */
class Snake[T](
                var XStart: Int = 0,
                var YStart: Int = 0,
                var ADeleted: Int = 0,
                var BInserted: Int = 0,
                var DiagonalLength: Int = 0,
                var IsForward: Boolean = true,
                var DELTA: Int = 0,
                var IsMiddle: Boolean = false,
                var D: Int = -1) {

    /**
     * Calculates a new snake segment depending on the current comparison direction.
     *
     * @param V  An array of end points for a given k-line
     * @param k  The k-line the snake should get calculated for
     * @param d  Number of differences for the same trace
     * @param pa Elements of the first object. Usually the original object
     * @param a0 The starting position in the array of elements from the first object to compare
     * @param N  The index of the last element from the first object to compare
     * @param pb Elements of the second object. Usually the current object
     * @param b0 The starting position in the array of elements from the second object to compare
     * @param M  The index of the last element from the second object to compare
     * @return The calculated snake segment
     */
    def Calculate(V: V, k: Int, d: Int, pa: Array[T], a0: Int, N: Int, pb: Array[T], b0: Int, M: Int) = {
        if (IsForward) CalculateForward(V, k, d, pa, a0, N, pb, b0, M)
        else CalculateBackward(V, k, d, pa, a0, N, pb, b0, M)
    }

    /**
     * Calculates a new snake segment for a forward comparison direction.
     *
     * @param V  An array of end points for a given k-line
     * @param k  The k-line the snake should get calculated for
     * @param d  Number of differences for the same trace
     * @param pa Elements of the first object. Usually the original object
     * @param a0 The starting position in the array of elements from the first object to compare
     * @param N  The index of the last element from the first object to compare
     * @param pb Elements of the second object. Usually the current object
     * @param b0 The starting position in the array of elements from the second object to compare
     * @param M  The index of the last element from the second object to compare
     * @return The calculated snake segment
     */
    private def CalculateForward(V: V, k: Int, d: Int, pa: Array[T], a0: Int, N: Int, pb: Array[T], b0: Int, M: Int) = {
        // determine if a insertion (down) or a deletion (right) occurred
        val down = (k == -d || (k != d && V.getK(k - 1) < V.getK(k + 1)))

        // calculate the x and y position based on the movement direction
        val xStart = if (down) V.getK(k + 1) else V.getK(k - 1)
        val yStart = xStart - (if(down) k + 1 else k - 1)

        // calculate the x and y position of the end point
        var xEnd = if (down) xStart else xStart + 1
        var yEnd = xEnd - k

        // calculate the number of equal elements in both objects for this
        // segment
        var snake = 0
        while (xEnd < N && yEnd < M && pa(xEnd + a0).equals(pb(yEnd + b0))) {
            xEnd += 1
            yEnd += 1
            snake += 1
        }

        // assign the calculated values to the fields of this instance
        XStart = xStart + a0
        YStart = yStart + b0
        ADeleted = if (down) 0 else 1
        BInserted = if (down) 1 else 0
        DiagonalLength = snake

        RemoveStubs(a0, N, b0, M)

        this
    }

    /**
     * Calculates a new snake segment for a backward comparison direction.
     *
     * @param V  An array of end points for a given k-line
     * @param k  The k-line the snake should get calculated for
     * @param d  Number of differences for the same trace
     * @param pa Elements of the first object. Usually the original object
     * @param a0 The starting position in the array of elements from the first object to compare
     * @param N  The index of the last element from the first object to compare
     * @param pb Elements of the second object. Usually the current object
     * @param b0 The starting position in the array of elements from the second object to compare
     * @param M  The index of the last element from the second object to compare
     * @return The calculated snake segment
     */
    private def CalculateBackward(V: V, k: Int, d: Int, pa: Array[T], a0: Int, N: Int, pb: Array[T], b0: Int, M: Int) = {
        // determine if a insertion (up) or a deletion (left) occurred
        val up = (k == d + DELTA || (k != -d + DELTA && V.getK(k - 1) < V.getK(k + 1)))

        // calculate the x and y position based on the movement direction
        val xStart = if(up) V.getK(k - 1) else V.getK(k + 1)
        val yStart = xStart - (if(up) k - 1 else k + 1)

        // calculate the x and y position of the end point
        var xEnd = if(up) xStart else xStart - 1
        var yEnd = xEnd - k

        // calculate the number of equal elements in both objects for this
        // segment by following diagonals
        var snake = 0
        while (xEnd > 0 && yEnd > 0 && pa(xEnd - 1).equals(pb(yEnd - 1))) {
            xEnd -= 1
            yEnd -= 1
            snake += 1
        }

        // assign the calculated values to the fields of this instance
        XStart = xStart
        YStart = yStart
        ADeleted = if (up) 0 else 1
        BInserted = if(up) 1 else 0
        DiagonalLength = snake

        RemoveStubs(a0, N, b0, M)

        this
    }

    /**
     * Returns the start point of this snake segment.
     *
     * @return The start point of this snake segment
     */
    def getStartPoint(): (Integer, Integer) = {
        (XStart, YStart)
    }

    /**
     * Returns the mid point of this snake segment.
     *
     * @return The mid point of this snake segment
     */
    def getMidPoint(): (Integer, Integer) = {
        (getXMid(), getYMid())
    }

    /**
     * Returns the end point of this snake segment.
     *
     * @return The end point of this snake segment
     */
    def getEndPoint(): (Integer, Integer) = {
        (getXEnd(), getYEnd())
    }

    /**
     * Returns the x-position of the mid point for this snake segment.
     *
     * @return The x-position of the mid point
     */
    def getXMid(): Int =  {
        if (IsForward) XStart + ADeleted
        else XStart - ADeleted
    }

    /**
     * Returns the y-position of the mid point for this snake segment.
     *
     * @return The y-position of the mid point
     */
    def getYMid(): Int =  {
        if (IsForward) YStart + BInserted
        else YStart - BInserted
    }

    /**
     * Returns the x-position of the end point for this snake segment.
     *
     * @return The x-position of the end point
     */
    def getXEnd(): Int =  {
        if (IsForward) XStart + ADeleted + DiagonalLength
        else XStart - ADeleted - DiagonalLength
    }

    /**
     * Returns the y-position of the end point for this snake segment.
     *
     * @return The y-position of the end point
     */
    def getYEnd(): Int = {
        if (IsForward) YStart + BInserted + DiagonalLength
        else YStart - BInserted - DiagonalLength
    }

    /**
     * Returns if this snake segment is a middle point.
     *
     * @return true indicates that this segment is a middle point; false that it is not a middle point
     */
    def isMiddlePoint(): Boolean = IsMiddle

    /**
     * Defines if this snake segment is a middle point.
     *
     * @param isMiddle true indicates that this segment is a middle point, false that it is not a middle point
     */
    def setMiddlePoint(isMiddle: Boolean): Unit = {
        IsMiddle = isMiddle
    }

    /**
     * Returns the number of differences between the first and the second object in that trace and for that segment.
     *
     * @return The number of differences in that trace
     */
    def getD() = D

    /**
     * Sets the d contours for this segment which correspond to the number of differences in that trace, irrespective of
     * the number of equal elements.
     *
     * @param d The number of differences in that trace
     */
    def setD(d: Int) = {
        D = d
    }

    override def toString() = {
        "Snake " +
          (if (IsForward) "F" else "R") + ": " +
                "( " + XStart + ", " + YStart + " ) + " +
          (if (ADeleted > 0) "D( " else (if (BInserted > 0) "I( " else "( ")) + +ADeleted + ", " + BInserted + " ) + " +
          DiagonalLength + " -> ( " + getXEnd() + ", " + getYEnd() + " )" +
          " k=" + (getXMid() - getYMid())
    }

    /**
     * Removes the effects of a single insertion (down or up movement in the graph) if the x-position of the starting
     * vertex equals <em>a0</em> and the y-position of the starting vertex equals the y-position of <em>b0</em> before
     * the insertion.
     *
     * @param a0 The starting position in the array of elements from the first object to compare
     * @param N  The index of the last element from the first object to compare
     * @param b0 The starting position in the array of elements from the second object to compare
     * @param M  The index of the last element from the second object to compare
     */
    private def RemoveStubs(a0: Int, N: Int, b0: Int, M: Int): Unit =
        if (IsForward) {
            if (XStart == a0 && YStart == b0 - 1 && BInserted == 1) {
                YStart = b0
                BInserted = 0
            }
        } else {
            if (XStart == a0 + N && YStart == b0 + M + 1 && BInserted == 1) {
                YStart = b0 + M
                BInserted = 0
            }
        }

    /**
     * Combines two snakes of the same kind to reduce the number of returned snakes.
     * <p>
     * A snake is of the same kind if both are in the same direction and if both have either a positive ADeleted field
     * or a positive BInserted field, but not either a positive ADeleted and the other a positive BInserted field!
     * Moreover, if the snake to append has a DiagonalLength > 0 it is not meant to be of the same kind and therefore
     * should not be appended to this snake.
     *
     * @param snake The snake to append to the current snake
     * @return true if the snake could be appended to this snake; false otherwise
     */
    def append(snake: Snake[T]): Boolean =
        if (((IsForward && DiagonalLength >= 0) || (!IsForward && snake.DiagonalLength >= 0)) &&
          (ADeleted > 0 && snake.ADeleted > 0 || BInserted > 0 && snake.BInserted > 0)) {
            ADeleted += snake.ADeleted
            BInserted += snake.BInserted

            DiagonalLength += snake.DiagonalLength

            if (IsForward) {
                XStart = Math.min(XStart, snake.XStart)
                YStart = Math.min(YStart, snake.YStart)
            } else {
                XStart = Math.max(XStart, snake.XStart)
                YStart = Math.max(YStart, snake.YStart)
            }
            true
        }
        else false

}

object Snake {

    /**
      * Initializes a new snake. The comparison direction can be defined via the <em>isForward</em> parameter. If set to
      * true, the comparison will be done from start till end, while a value of false will result in a backward
      * comparison from end to start.
      * <p>
      * <em>delta</em> defines the difference in length between the first and the second object to compare.
      *
      * @param isForward If set to true a forward comparison will be done; else a backward comparison
      * @param delta     The difference in length between the first and the second object to compare
      */
    def apply[T](isForward: Boolean, delta: Int): Snake[T] =
        new Snake[T](IsForward = isForward, DELTA = if (isForward) 0 else delta)

    /**
      * Initializes a new snake segment.
      *
      * @param a0        The starting position in the array of elements from the first object to compare
      * @param N         The index of the last element from the first object to compare
      * @param b0        The starting position in the array of elements from the second object to compare
      * @param M         The index of the last element from the second object to compare
      * @param isForward The comparison direction; true for a forward comparison, false otherwise
      * @param xStart    The x-position of the current node
      * @param yStart    The y-position of the current node
      * @param aDeleted  Defines the number of removed elements from the first object (right movements in the graph)
      * @param bInserted Defines the number of inserted elements from the second object (down movement in the graph)
      * @param diagonal  Defines the number of equal elements in both objects for a given segment
      */
    def apply[T](a0: Int, N: Int, b0: Int, M: Int, isForward: Boolean, xStart: Int, yStart: Int, aDeleted: Int, bInserted: Int, diagonal: Int): Snake[T] = {
        val snake = new Snake[T](
        XStart = xStart,
        YStart = yStart,
        ADeleted = aDeleted,
        BInserted = bInserted,
        DiagonalLength = diagonal,
        IsForward = isForward)
        snake.RemoveStubs(a0, N, b0, M)
        snake
    }

    /**
      * Initializes a new snake segment.
      *
      * @param a0        The starting position in the array of elements from the first object to compare
      * @param N         The index of the last element from the first object to compare
      * @param b0        The starting position in the array of elements from the second object to compare
      * @param M         The index of the last element from the second object to compare
      * @param isForward The comparison direction; true for a forward comparison, false otherwise
      * @param xStart    The x-position of the current node
      * @param yStart    The y-position of the current node
      * @param down      Defines if insertion (down movement; true) or a deletion (right movement; false) should be done
      * @param diagonal  Defines the number of equal elements in both objects for a given segment
      */
    def apply[T](a0: Int, N: Int, b0: Int, M: Int, isForward: Boolean, xStart: Int, yStart: Int, down: Boolean, diagonal: Int): Snake[T] = {
        val snake = new Snake[T](
        XStart = xStart,
        YStart = yStart,
        ADeleted = if (down) 0 else 1,
        BInserted = if (down) 1 else 0,
        DiagonalLength = diagonal,
        IsForward = isForward)
        snake.RemoveStubs(a0, N, b0, M)
        snake
    }

    /**
      * Initializes a new instance and calculates the segment based on the provided data.
      *
      * @param a0      The starting position in the array of elements from the first object to compare
      * @param N       The index of the last element from the first object to compare
      * @param b0      The starting position in the array of elements from the second object to compare
      * @param M       The index of the last element from the second object to compare
      * @param forward The comparison direction; true for a forward comparison, false otherwise
      * @param delta   The difference in length of both objects to compare
      * @param V       An array of end points for a given k-line
      * @param k       The k-line the snake should get calculated for
      * @param d       Number of differences for the same trace
      * @param pa      Elements of the first object. Usually the original object
      * @param pb      Elements of the second object. Usually the current object
      */
    def apply[T](a0: Int, N: Int, b0: Int, M: Int, forward: Boolean, delta: Int, V: V, k: Int, d: Int, pa: Array[T], pb: Array[T]): Snake[T] = {
        val snake = apply[T](forward, delta)
        snake.Calculate(V, k, d, pa, a0, N, pb, b0, M)
    }



}