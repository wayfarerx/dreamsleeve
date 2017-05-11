package net.wayfarerx.dreamsleeve.diff

/**
 * A snake is a segment along a path which converts an object A to object B by either eliminating elements from object A
 * or inserting elements from object B.
 *
 * @param xStart   The x-position of a starting point
 * @param yStart   The y-position of a starting point
 * @param deleted  Defines the number of deleted elements from the first object to match the second object
 * @param inserted Defines the number of inserted elements from the second object to match the first object
 * @param diagonal Defines the number of equal elements in both objects
 * @param forward  Defines the comparison direction of both objects
 * @author Roman Vottner
 */
private[diff] class AOldSnake private(
  val xStart: Int,
  val yStart: Int,
  val deleted: Int,
  val inserted: Int,
  val diagonal: Int,
  val forward: Boolean) {

  /**
   * Returns the x-position of the mid point for this snake segment.
   *
   * @return The x-position of the mid point
   */
  def xMid: Int = {
    if (forward) xStart + deleted
    else xStart - deleted
  }

  /**
   * Returns the y-position of the mid point for this snake segment.
   *
   * @return The y-position of the mid point
   */
  def yMid: Int = {
    if (forward) yStart + inserted
    else yStart - inserted
  }

  /**
   * Returns the x-position of the end point for this snake segment.
   *
   * @return The x-position of the end point
   */
  def xEnd: Int = {
    if (forward) xStart + deleted + diagonal
    else xStart - deleted - diagonal
  }

  /**
   * Returns the y-position of the end point for this snake segment.
   *
   * @return The y-position of the end point
   */
  def yEnd: Int = {
    if (forward) yStart + inserted + diagonal
    else yStart - inserted - diagonal
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
   * @return The combined snake if the snake could be appended to this snake; None otherwise
   */
  def append(snake: AOldSnake): Option[AOldSnake] =
    if (diagonal >= 0 && (deleted > 0 && snake.deleted > 0 || inserted > 0 && snake.inserted > 0)) Some {
      if (forward) new AOldSnake(Math.min(xStart, snake.xStart), Math.min(yStart, snake.yStart), deleted + snake.deleted, inserted + snake.inserted, diagonal + snake.diagonal, forward)
      else new AOldSnake(Math.max(xStart, snake.xStart), Math.max(yStart, snake.yStart), deleted + snake.deleted, inserted + snake.inserted, diagonal + snake.diagonal, forward)
    } else None

}

private[diff] object AOldSnake {

  /**
   * Initializes a new snake segment.
   *
   * @param a0       The starting position in the array of elements from the first object to compare
   * @param N        The index of the last element from the first object to compare
   * @param b0       The starting position in the array of elements from the second object to compare
   * @param M        The index of the last element from the second object to compare
   * @param forward  The comparison direction; true for a forward comparison, false otherwise
   * @param xStart   The x-position of the current node
   * @param yStart   The y-position of the current node
   * @param down     Defines if insertion (down movement; true) or a deletion (right movement; false) should be done
   * @param diagonal Defines the number of equal elements in both objects for a given segment
   */
  def apply(a0: Int, N: Int, b0: Int, M: Int, forward: Boolean, xStart: Int, yStart: Int, down: Boolean, diagonal: Int): AOldSnake =
    create(
      xStart,
      yStart,
      if (down) 0 else 1,
      if (down) 1 else 0,
      diagonal,
      forward,
      a0,
      N,
      b0,
      M)

  /**
   * Initializes a new snake segment.
   *
   * @param a0       The starting position in the array of elements from the first object to compare
   * @param N        The index of the last element from the first object to compare
   * @param b0       The starting position in the array of elements from the second object to compare
   * @param M        The index of the last element from the second object to compare
   * @param forward  The comparison direction; true for a forward comparison, false otherwise
   * @param xStart   The x-position of the current node
   * @param yStart   The y-position of the current node
   * @param deleted  Defines the number of removed elements from the first object (right movements in the graph)
   * @param inserted Defines the number of inserted elements from the second object (down movement in the graph)
   * @param diagonal Defines the number of equal elements in both objects for a given segment
   */
  def apply(a0: Int, N: Int, b0: Int, M: Int, forward: Boolean, xStart: Int, yStart: Int, deleted: Int, inserted: Int, diagonal: Int): AOldSnake =
    create(
      xStart,
      yStart,
      deleted,
      inserted,
      diagonal,
      forward,
      a0,
      N,
      b0,
      M)

  private def create(
    xStart: Int,
    yStart: Int,
    deleted: Int,
    inserted: Int,
    diagonal: Int,
    forward: Boolean,
    a0: Int,
    N: Int,
    b0: Int,
    M: Int
  ): AOldSnake = {
    var _yStart = yStart
    var _inserted = inserted
    if (forward) {
      if (xStart == a0 && _yStart == b0 - 1 && _inserted == 1) {
        _yStart = b0
        _inserted = 0
      }
    } else {
      if (xStart == a0 + N && _yStart == b0 + M + 1 && _inserted == 1) {
        _yStart = b0 + M
        _inserted = 0
      }
    }
    new AOldSnake(
      xStart,
      _yStart,
      deleted,
      _inserted,
      diagonal,
      forward)
  }


}

//
//
//


/**
 * Base class for all segments along a path which converts an object A to object B by either eliminating elements from
 * object A or inserting elements from object B.
 *
 * @author wayfarerx
 * @author Roman Vottner
 */
private[diff] sealed trait Snake {

  /** The concrete type of this snake. */
  type SnakeType <: Snake

  /** The x-position of a starting point. */
  def xStart: Int

  /** The y-position of a starting point. */
  def yStart: Int

  /** The x-position of a middle point. */
  def xMid: Int

  /** The y-position of a middle point. */
  def yMid: Int

  /** The x-position of a end point. */
  def xEnd: Int

  /** The y-position of a end point. */
  def yEnd: Int

  /** The number of deleted elements from the first object to match the second object. */
  def deleted: Int

  /** The number of inserted elements from the second object to match the first object. */
  def inserted: Int

  /** The number of equal elements in both objects. */
  def diagonal: Int

  /** The comparison direction of both objects. */
  def forward: Boolean

  /**
   * Combines two snakes of the same kind to reduce the number of returned snakes.
   *
   * A snake is of the same kind if  both have either a positive number of deleted elements or a positive number of
   * inserted elements but not both inserted and deleted elements. Moreover, if either snake to append has a diagonal of
   * length greater than zero they are not meant to be of the same kind and therefore should not be appended.
   *
   * @param snake The snake to append to the current snake.
   * @return The combined snake if the snake could be appended to this snake; None otherwise.
   */
  final def append(snake: Snake): Option[Snake] =
    if ((diagonal == 0 && snake.diagonal == 0) &&
      (deleted > 0 && snake.deleted > 0 || inserted > 0 && snake.inserted > 0) &&
      (deleted == 0 && snake.deleted == 0 || inserted == 0 && snake.inserted == 0)) {
      Some(appendSnake(snake))
    } else None


  /**
   * Combines two snakes of the same kind to reduce the number of returned snakes.
   *
   * @param snake The snake to append to the current snake.
   * @return The combined snake.
   */
  protected def appendSnake(snake: Snake): SnakeType

}

/**
 * Implementations of forward and reverse snakes.
 *
 * @author wayfarerx
 * @author Roman Vottner
 */
private[diff] object Snake {

  /**
   * A snake that moves from (0, 0) to (M, N).
   *
   * @param xStart   The x-position of a starting point.
   * @param yStart   The y-position of a starting point.
   * @param deleted  The number of deleted elements from the first object to match the second object.
   * @param inserted The number of inserted elements from the second object to match the first object.
   * @param diagonal The number of equal elements in both objects.
   */
  case class Forward private(
    xStart: Int,
    yStart: Int,
    deleted: Int,
    inserted: Int,
    diagonal: Int
  ) extends Snake {

    /* Set the concrete snake type. */
    override type SnakeType = Forward

    /* This is a forward snake. */
    override def forward = true

    /* Calculate the x-position of a middle point. */
    override def xMid: Int = xStart + deleted

    /* Calculate the y-position of a middle point. */
    override def yMid: Int = yStart + inserted

    /* Calculate the x-position of a end point. */
    override def xEnd: Int = xMid + diagonal

    /* Calculate the y-position of a end point. */
    override def yEnd: Int = yMid + diagonal

    /* Combine two snakes of the same kind to reduce the number of returned snakes. */
    override protected def appendSnake(snake: Snake) =
      Forward(
        Math.min(xStart, snake.xStart),
        Math.min(yStart, snake.yStart),
        deleted + snake.deleted,
        inserted + snake.inserted,
        diagonal + snake.diagonal)

  }

  /**
   * A factory for forward snakes.
   */
  object Forward extends Factory {

    /* Set the concrete snake type. */
    override type SnakeType = Forward

    /**
     * Creates a forward snake with the specified parameters.
     *
     * @param xStart   The x-position of a starting point.
     * @param yStart   The y-position of a starting point.
     * @param deleted  The number of deleted elements from the first object to match the second object.
     * @param inserted The number of inserted elements from the second object to match the first object.
     * @param diagonal The number of equal elements in both objects.
     * @param a0       The starting position in the array of elements from the first object to compare.
     * @param N        The index of the last element from the first object to compare.
     * @param b0       The starting position in the array of elements from the second object to compare.
     * @param M        The index of the last element from the second object to compare.
     * @return A forward snake with the specified parameters.
     */
    override protected def create(
      xStart: Int,
      yStart: Int,
      deleted: Int,
      inserted: Int,
      diagonal: Int,
      a0: Int,
      N: Int,
      b0: Int,
      M: Int
    ): Forward = {
      var _yStart = yStart
      var _inserted = inserted
      if (xStart == a0 && _yStart == b0 - 1 && _inserted == 1) {
        _yStart = b0
        _inserted = 0
      }
      Forward(xStart, _yStart, deleted, _inserted, diagonal)
    }

  }

  /**
   * A snake that moves from (M, N) to (0, 0).
   *
   * @param xStart   The x-position of a starting point.
   * @param yStart   The y-position of a starting point.
   * @param deleted  The number of deleted elements from the first object to match the second object.
   * @param inserted The number of inserted elements from the second object to match the first object.
   * @param diagonal The number of equal elements in both objects.
   */
  case class Backward private(
    xStart: Int,
    yStart: Int,
    deleted: Int,
    inserted: Int,
    diagonal: Int
  ) extends Snake {

    /* Set the concrete snake type. */
    override type SnakeType = Backward

    /* This is a reverse snake. */
    override def forward = false

    /* Calculate the x-position of a middle point. */
    override def xMid: Int = xStart - deleted

    /* Calculate the y-position of a middle point. */
    override def yMid: Int = yStart - inserted

    /* Calculate the x-position of a end point. */
    override def xEnd: Int = xMid - diagonal

    /* Calculate the y-position of a end point. */
    override def yEnd: Int = yMid - diagonal

    /* Combine two snakes of the same kind to reduce the number of returned snakes. */
    override protected def appendSnake(snake: Snake) =
      Backward(
        Math.min(xStart, snake.xStart),
        Math.min(yStart, snake.yStart),
        deleted + snake.deleted,
        inserted + snake.inserted,
        diagonal + snake.diagonal)

  }

  /**
   * A factory for reverse snakes.
   */
  object Backward extends Factory {

    /* Set the concrete snake type. */
    override type SnakeType = Backward

    /**
     * Creates a reverse snake with the specified parameters.
     *
     * @param xStart   The x-position of a starting point.
     * @param yStart   The y-position of a starting point.
     * @param deleted  The number of deleted elements from the first object to match the second object.
     * @param inserted The number of inserted elements from the second object to match the first object.
     * @param diagonal The number of equal elements in both objects.
     * @param a0       The starting position in the array of elements from the first object to compare.
     * @param N        The index of the last element from the first object to compare.
     * @param b0       The starting position in the array of elements from the second object to compare.
     * @param M        The index of the last element from the second object to compare.
     * @return A reverse snake with the specified parameters.
     */
    override protected def create(
      xStart: Int,
      yStart: Int,
      deleted: Int,
      inserted: Int,
      diagonal: Int,
      a0: Int,
      N: Int,
      b0: Int,
      M: Int
    ): Backward = {
      var _yStart = yStart
      var _inserted = inserted
      if (xStart == a0 + N && _yStart == b0 + M + 1 && _inserted == 1) {
        _yStart = b0 + M
        _inserted = 0
      }
      Backward(xStart, _yStart, deleted, _inserted, diagonal)
    }

  }

  sealed trait Factory {

    type SnakeType <: Snake

    /**
     * Initializes a new snake segment.
     *
     * @param a0       The starting position in the array of elements from the first object to compare
     * @param N        The index of the last element from the first object to compare
     * @param b0       The starting position in the array of elements from the second object to compare
     * @param M        The index of the last element from the second object to compare
     * @param xStart   The x-position of the current node
     * @param yStart   The y-position of the current node
     * @param down     Defines if insertion (down movement; true) or a deletion (right movement; false) should be done
     * @param diagonal Defines the number of equal elements in both objects for a given segment
     */
    def apply(a0: Int, N: Int, b0: Int, M: Int, xStart: Int, yStart: Int, down: Boolean, diagonal: Int): SnakeType =
      create(
        xStart,
        yStart,
        if (down) 0 else 1,
        if (down) 1 else 0,
        diagonal,
        a0,
        N,
        b0,
        M)

    /**
     * Initializes a new snake segment.
     *
     * @param a0       The starting position in the array of elements from the first object to compare
     * @param N        The index of the last element from the first object to compare
     * @param b0       The starting position in the array of elements from the second object to compare
     * @param M        The index of the last element from the second object to compare
     * @param xStart   The x-position of the current node
     * @param yStart   The y-position of the current node
     * @param deleted  Defines the number of removed elements from the first object (right movements in the graph)
     * @param inserted Defines the number of inserted elements from the second object (down movement in the graph)
     * @param diagonal Defines the number of equal elements in both objects for a given segment
     */
    def apply(a0: Int, N: Int, b0: Int, M: Int, xStart: Int, yStart: Int, deleted: Int, inserted: Int, diagonal: Int): SnakeType =
      create(
        xStart,
        yStart,
        deleted,
        inserted,
        diagonal,
        a0,
        N,
        b0,
        M)

    protected def create(
      xStart: Int,
      yStart: Int,
      deleted: Int,
      inserted: Int,
      diagonal: Int,
      a0: Int,
      N: Int,
      b0: Int,
      M: Int
    ): SnakeType

  }

}
