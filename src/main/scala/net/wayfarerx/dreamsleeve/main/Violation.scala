package net.wayfarerx.dreamsleeve.main

class Violation(message: String) extends RuntimeException(message)

object Violation {
  
  def apply(message: String): Violation = new Violation(message)
  
}