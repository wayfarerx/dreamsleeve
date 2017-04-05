package net.wayfarerx.dreamsleeve.main

/**
 * Representation of the options that can be specified for commands.
 *
 * @param sourcePath The path of the ESO saved variables folder.
 * @param targetPath The path of the dreamsleeve database.
 * @param showHelp True if the help message should be shown.
 */
case class Options(
    sourcePath: String,
    targetPath: String,
    showHelp: Boolean) {

  /**
   * Configures an option and reads any of its arguments from the specified iterator.
   *
   * @param key The identifier for the option to set.
   * @param args The arguments to pull from.
   * @return A copy of these options overwritten with the specified option.
   */
  def readOption(key: String, args: Iterator[String]): Options = key match {
    case "-f" | "--from" =>
      if (args.hasNext) copy(sourcePath = args.next())
      else throw Violation(s"Option $key requires an argument specifying the path of the ESO saved variables folder.")
    case "-t" | "--to" =>
      if (args.hasNext) copy(targetPath = args.next())
      else throw Violation(s"Option $key requires an argument specifying the path of the dreamsleeve database.")
    case "--help" =>
      copy(showHelp = true)
    case _ =>
      throw Violation(s"Invalid option key: $key.")
  }

}

/**
 * Factory for the default options.
 */
object Options {

  /** The default, platform-dependent options. */
  val Defaults = {
    Options("", "", false) // FIXME
  }

}