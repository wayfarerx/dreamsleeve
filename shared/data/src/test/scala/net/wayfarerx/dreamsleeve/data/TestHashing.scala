package net.wayfarerx.dreamsleeve.data

import java.security.MessageDigest

object TestHashing {

  /** The message digest to use. */
  private val digest = MessageDigest.getInstance("SHA-256")

  /**
   * Generates a hash for one hash component.
   *
   * @param c0 The only component to hash.
   * @tparam C0 The type of the only component to hash.
   * @return The hash of the one component.
   */
  def apply[C0: Component](c0: C0): Hash = {
    implicitly[Component[C0]].apply(c0, digest)
    Hash.setInternalRepresentation(digest.digest())
  }

  /**
   * Generates a hash for two hash components.
   *
   * @param c0 The first component to hash.
   * @param c1 The second component to hash.
   * @tparam C0 The type of the first component to hash.
   * @tparam C1 The type of the second component to hash.
   * @return The hash of the two components.
   */
  def apply[C0: Component, C1: Component](c0: C0, c1: C1): Hash = {
    implicitly[Component[C0]].apply(c0, digest)
    implicitly[Component[C1]].apply(c1, digest)
    Hash.setInternalRepresentation(digest.digest())
  }

  /**
   * Generates a hash for three hash components.
   *
   * @param c0 The first component to hash.
   * @param c1 The second component to hash.
   * @param c2 The third component to hash.
   * @tparam C0 The type of the first component to hash.
   * @tparam C1 The type of the second component to hash.
   * @tparam C2 The type of the third component to hash.
   * @return The hash of the three components.
   */
  def apply[C0: Component, C1: Component, C2: Component](c0: C0, c1: C1, c2: C2): Hash = {
    implicitly[Component[C0]].apply(c0, digest)
    implicitly[Component[C1]].apply(c1, digest)
    implicitly[Component[C2]].apply(c2, digest)
    Hash.setInternalRepresentation(digest.digest())
  }

  /**
   * Generates a hash for four hash components.
   *
   * @param c0 The first component to hash.
   * @param c1 The second component to hash.
   * @param c2 The third component to hash.
   * @param c3 The fourth component to hash.
   * @tparam C0 The type of the first component to hash.
   * @tparam C1 The type of the second component to hash.
   * @tparam C2 The type of the third component to hash.
   * @tparam C3 The type of the fourth component to hash.
   * @return The hash of the four components.
   */
  def apply[C0: Component, C1: Component, C2: Component, C3: Component](c0: C0, c1: C1, c2: C2, c3: C3): Hash = {
    implicitly[Component[C0]].apply(c0, digest)
    implicitly[Component[C1]].apply(c1, digest)
    implicitly[Component[C2]].apply(c2, digest)
    implicitly[Component[C3]].apply(c3, digest)
    Hash.setInternalRepresentation(digest.digest())
  }

  /**
   * Base class for hashed component type classes.
   *
   * @tparam T The type of the underlying hashed component.
   */
  sealed trait Component[-T] {

    /**
     * Appends the specified hashed component to a message digest.
     *
     * @param component The hashed component to append.
     * @param digest    The message digest to append to.
     */
    def apply(component: T, digest: MessageDigest): Unit

  }

  /**
   * Implementations of the supported hashed components.
   */
  object Component {

    /** Support for booleans as hashed components. */
    implicit val Booleans: Component[Boolean] = new Component[Boolean] {
      override def apply(c: Boolean, d: MessageDigest): Unit =
        d.update(if (c) 0xFF.toByte else 0x00.toByte)
    }

    /** Support for bytes as hashed components. */
    implicit val Bytes: Component[Byte] = new Component[Byte] {
      override def apply(c: Byte, d: MessageDigest): Unit =
        d.update(c)
    }

    /** Support for shorts as hashed components. */
    implicit val Shorts: Component[Short] = new Component[Short] {
      override def apply(c: Short, d: MessageDigest): Unit =
        for (i <- 0 to 1) d.update((c >>> (1 - i) * 8 & 0x00FF).toByte)
    }

    /** Support for chars as hashed components. */
    implicit val Chars: Component[Char] = new Component[Char] {
      override def apply(c: Char, d: MessageDigest): Unit =
        for (i <- 0 to 1) d.update((c >>> (1 - i) * 8 & 0x00FF).toByte)
    }

    /** Support for ints as hashed components. */
    implicit val Ints: Component[Int] = new Component[Int] {
      override def apply(c: Int, d: MessageDigest): Unit =
        for (i <- 0 to 3) d.update((c >>> (3 - i) * 8 & 0x000000FF).toByte)
    }

    /** Support for floats as hashed components. */
    implicit val Floats: Component[Float] = new Component[Float] {
      override def apply(c: Float, d: MessageDigest): Unit =
        Ints(java.lang.Float.floatToIntBits(c), d)
    }

    /** Support for longs as hashed components. */
    implicit val Longs: Component[Long] = new Component[Long] {
      override def apply(c: Long, d: MessageDigest): Unit =
        for (i <- 0 to 7) d.update((c >>> (7 - i) * 8 & 0x00000000000000FF).toByte)
    }

    /** Support for doubles as hashed components. */
    implicit val Doubles: Component[Double] = new Component[Double] {
      override def apply(c: Double, d: MessageDigest): Unit =
        Longs(java.lang.Double.doubleToLongBits(c), d)
    }

    /** Support for strings as hashed components. */
    implicit val Strings: Component[String] = new Component[String] {
      override def apply(c: String, d: MessageDigest): Unit =
        d.update(c.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    }

    /** Support for hashes as hashed components. */
    implicit val Hashes: Component[Hash] = new Component[Hash] {
      override def apply(c: Hash, d: MessageDigest): Unit =
        d.update(Hash.getInternalRepresentation(c))
    }

    /** Support for collections of hashes. */
    implicit val MultipleHashes: Component[Iterable[Hash]] = new Component[Iterable[Hash]] {
      override def apply(c: Iterable[Hash], d: MessageDigest): Unit =
        for (i <- c) implicitly[Component[Hash]].apply(i, d)
    }

  }

}
