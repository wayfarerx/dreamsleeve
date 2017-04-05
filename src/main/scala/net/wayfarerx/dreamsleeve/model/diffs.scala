package net.wayfarerx.dreamsleeve.model

trait Diff {
  
}

class TableDiff extends Diff {
  
}

object TableDiff {
  
  case class Insert(index: Int, key: Value, value: Node)
  
  case class Replace(index: Int, replacement: Diff)
  
  case class Remove(index: Int)
  
}