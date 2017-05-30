package net.wayfarerx.dreamsleeve
package parser


import collection.immutable.SortedMap
import org.parboiled2._
import model._

/**
 * A parser for tables written to text files.
 *
 * @param input The input to this parser.
 */
class DocumentParser(val input: ParserInput) extends Parser {

  import model.Node

  //
  // Entry point.
  //

  def Document: Rule1[Node.Document] = rule {
    Whitespace ~ Title ~ Equals ~ (Boolean | Number | String | Table) ~ EOI ~> { (t: String, n: Node) =>
      Node.Document(t, n)
    }
  }

  //
  // Composite rules.
  //

  //def Table: Rule1[Table] = LeftBracket ~ zeroOrMore {
  //  ???
  //} ~ RightBracket

  def Key: Rule1[Node.Value] = rule {
    LeftBracket ~ (Boolean | Number | String) ~ RightBracket
  }

  def Entry: Rule1[(Node.Value, Node)] = rule {
    (Key ~ Equals ~ (Boolean | Number | String | Table)) ~> { (k: Node.Value, n: Node) => k -> n }
  }

  def Entries: Rule1[Seq[(Node.Value, Node)]] = rule {
    Entry ~ zeroOrMore(Comma ~ Entry) ~ optional(Comma) ~> { (h: (Node.Value, Node), t: Seq[(Node.Value, Node)]) =>
      h +: t
    }
  }

  def Table: Rule1[Node.Table] = rule {
    LeftBrace ~ optional(Entries) ~ RightBrace ~> { t: Option[Seq[(Node.Value, Node)]] =>
      Node.Table(SortedMap((t getOrElse Seq.empty): _*))
    }
  }

  //
  // Terminal rules.
  //

  def Title: Rule1[String] = rule {
    capture(atomic {
      (CharPredicate.Alpha | '_') ~ zeroOrMore(CharPredicate.Alpha | CharPredicate.Digit | '_')
    }) ~ Whitespace
  }

  def Boolean: Rule1[Node.Value.Boolean] = rule {
    capture(atomic("true" | "false")) ~> { (v: String) =>
      Node.Value.Boolean(v.toBoolean)
    } ~ Whitespace
  }

  def Number: Rule1[Node.Value.Number] = rule {
    capture(atomic {
      optional('-') ~ oneOrMore(CharPredicate.Digit) ~ optional(ch('.') ~ oneOrMore(CharPredicate.Digit))
    }) ~> { (v: String) =>
      Node.Value.Number(java.lang.Double.parseDouble(v))
    } ~ Whitespace
  }

  def String: Rule1[Node.Value.String] = rule {
    ch('"') ~ capture(zeroOrMore(noneOf("\"\\") | ("\\" ~ "\"") | ("\\" ~ "\\"))) ~ '"' ~> { (v: String) =>
      Node.Value.String(v.replaceAll("""\"""", """"""").replaceAll("""\\""", """\"""))
    } ~ Whitespace
  }

  def Comma: Rule0 = rule {
    ch(',') ~ Whitespace
  }

  def Equals: Rule0 = rule {
    ch('=') ~ Whitespace
  }

  def LeftBracket: Rule0 = rule {
    ch('[') ~ Whitespace
  }

  def RightBracket: Rule0 = rule {
    ch(']') ~ Whitespace
  }

  def LeftBrace: Rule0 = rule {
    ch('{') ~ Whitespace
  }

  def RightBrace: Rule0 = rule {
    ch('}') ~ Whitespace
  }

  //
  // Whitespace rules.
  //

  def Whitespace: Rule0 = rule {
    zeroOrMore {
      ch(' ') | '\t' | '\r' | '\n'
    }
  }

}
