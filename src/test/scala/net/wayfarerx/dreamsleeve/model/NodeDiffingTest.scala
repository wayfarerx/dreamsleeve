package net.wayfarerx.dreamsleeve.model

import org.scalatest._
import org.parboiled2.ParserInput

import net.wayfarerx.dreamsleeve.parser.DocumentParser

/**
 * Test case for the node hashing implementation.
 */
class NodeDiffingTest extends FlatSpec with Matchers {

  import Node._

  val A =
    """
      MM00DataSavedVariables =
      {
        [2] =
        {
            ["timestamp"] = 1494658163,
            ["seller"] = "@ua02047",
            ["buyer"] = "@zekak",
            ["itemLink"] = "|H0:item:43009:362:50:0:0:0:0:0:0:0:0:0:0:0:0:3:0:0:0:0:0|h|h",
            ["id"] = "755754105",
            ["guild"] = "Iron Bank of Bravos",
            ["quant"] = 1,
            ["wasKiosk"] = true,
            ["price"] = 399,
        }
      }
    """.getBytes("UTF-8")
  val B =
    """
      MM00DataSavedVariables =
      {
        [2] =
        {
            ["wasKiosk"] = true,
            ["buyer"] = "@zekak",
            ["guild"] = "Iron Bank of Bravos",
            ["itemLink"] = "|H0:item:43009:362:50:0:0:0:0:0:0:0:0:0:0:0:0:3:0:0:0:0:0|h|h",
            ["seller"] = "@ua02047",
            ["quant"] = 1,
            ["price"] = 399,
            ["timestamp"] = 1494658163,
            ["id"] = "755754105",
        }
      }
    """.getBytes("UTF-8")

  "All nodes" should "produce consistent hashes" in {
    val a = load(A)
    Diff.Revise(a, a).apply(a) shouldBe a
    val b = load(B)
    Diff.Revise(a, b).apply(a) shouldBe b
  }

  it should "produce consistent hashes fom big files" in {
    val a = loadFrom("MM00Data-0.lua")
    Diff.Revise(a, a).apply(a) shouldBe a
    val b = loadFrom("MM00Data-1.lua")
    Diff.Revise(a, b).apply(a) shouldBe b
  }

  /** Loads a document from a file. */
  def loadFrom(file: String): Document = {
    val resource = getClass.getResource(file)
    val connection = resource.openConnection()
    val data = new Array[Byte](connection.getContentLength)
    val input = connection.getInputStream
    try input.read(data) finally input.close()
    load(data)
  }

  /** Loads a document. */
  private def load(data: Array[Byte]) = {
    new DocumentParser(ParserInput(data)).Document.run().get
  }

  /*
  private def show(document: Document): Unit = {
    def show(node: Node, depth: Int): Unit = node match {
      case Table(entries) =>
        println(s"Table(${node.hash.toShortString}) {")
        val deeper = depth + 1
        for ((k, v) <- entries) {
          print("  " * deeper)
          show(k, deeper)
          print(" = ")
          show(v, deeper)
          println(",")
        }
        print("  " * depth + "}")
      case Value.String(s) =>
        print(s"String($s, ${node.hash.toShortString})")
      case Value.Number(n) =>
        print(s"Number($n, ${node.hash.toShortString})")
      case Value.Boolean(b) =>
        print(s"Boolean($b, ${node.hash.toShortString})")
    }

    print(s"Document(${document.title}, ${document.hash.toShortString}) = ")
    show(document.content, 0)
    println()
  }

  private def show(diff: Diff): Unit = {
    def showEdit(edit: Edit, depth: Int): Unit = edit match {
      case Edit.Insert(is) =>
        println(s"Insert [")
        val deeper = depth + 1
        for ((v, c) <- is) {
          print("  " * deeper + s"$v -> ")
          showChange(c, deeper)
          println(",")
        }
        print("  " * depth + "]")
      case Edit.Retain(is) =>
        println(s"Retain [")
        val deeper = depth + 1
        for ((h, c) <- is) {
          print("  " * deeper + s"${h.toShortString} -> ")
          showChange(c, deeper)
          println(",")
        }
        print("  " * depth + "]")
      case Edit.Delete(is) =>
        println(s"Delete [")
        val deeper = depth + 1
        for (h <- is) {
          println("  " * deeper + s"${h.toShortString},")
        }
        print("  " * depth + "]")
    }
    def showChange(change: Change, depth: Int): Unit = change match {
      case Change.Add(n) =>
        print(s"Add(${n.hash.toShortString})")
      case Change.Copy(h) =>
        print(s"Copy(${h.toShortString})")
      case Change.Replace(h, n) =>
        print(s"Replace(${h.toShortString}, ${n.hash.toShortString})")
      case Change.Modify(h, es) =>
        println(s"Modify(${h.toShortString}) {")
        val deeper = depth + 1
        for (e <- es) {
          print("  " * deeper)
          showEdit(e, deeper)
          println(",")
        }
        print("  " * depth + "}")
    }

    diff match {
      case Diff.Create(d) =>
        print(s"Create(${d.hash.toShortString})")
      case Diff.Revise(h, t, c) =>
        print(s"Revise(${h.toShortString}, $t) = ")
        showChange(c, 0)
      case Diff.Remove(h) =>
        print(s"Remove(${h.toShortString})")
    }
    println()
  }
  */
}