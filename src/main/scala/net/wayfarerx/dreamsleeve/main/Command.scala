package net.wayfarerx.dreamsleeve.main

/**
 * Base class for all external commands.
 */
trait Command extends (() => Int)

/**
 * Factory for command implementations.
 */
object Command {

  /**
   * Creates a command, including the options, from the specified command-line arguments.
   *
   * @param args The command-line arguments.
   * @return A command configured from the supplied command-line arguments.
   */
  def apply(args: Array[String]): Command = try {
    val iter = args.iterator
    var command: Option[String] = None
    var options = Options.Defaults
    while (iter.hasNext) {
      iter.next() match {
        case option if option startsWith "-" =>
          options = options.readOption(option, iter)
        case cmd if command.isEmpty =>
          command = Some(cmd)
        case unknown =>
          throw Violation(s"Invalid argument: $unknown.")
      }
    }
    command match {
      case Some(cmd) if options.showHelp => Help(Some(cmd))
      case Some("list") => List(options)
      case Some("snapshot") => Snapshot(options)
      case Some("restore") => Restore(options)
      case Some(unknown) => throw Violation(s"Invalid command: $unknown.")
      case None => Help(None)
    }
  } catch {
    case v: Violation => Help(Some(v.getMessage))
  }

  /**
   * The help command.
   *
   * @param cause The reason that the help command should be displayed.
   */
  case class Help(cause: Option[String]) extends Command {

    /* Runs this command. */
    override def apply() = {
      0 // FIXME
    }

  }

  /**
   * The list command.
   *
   * @param options The options to configure the command with.
   */
  case class List(options: Options) extends Command {

    /* Runs this command. */
    override def apply() = {
      0 // FIXME
    }

  }

  /**
   * The backup command.
   *
   * @param options The options to configure the command with.
   */
  case class Snapshot(options: Options) extends Command {

    /* Runs this command. */
    override def apply() = {
      0 // FIXME
    }

  }

  /**
   * The restore command.
   *
   * @param options The options to configure the command with.
   */
  case class Restore(options: Options) extends Command {

    /* Runs this command. */
    override def apply() = {
      0 // FIXME
    }

  }

}