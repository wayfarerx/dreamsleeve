package net.wayfarerx.dreamsleeve.main

/**
 * Application entry point.
 */
object Dreamsleeve extends App {

  {
    var status = 1
    try status = Command(args)()
    catch { case t: Throwable => t.printStackTrace() }
    finally System.exit(status)
  }

}