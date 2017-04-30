package net.wayfarerx.dreamsleeve
package parser


import scala.collection.immutable.ListMap
import org.parboiled2._
import model._

/**
 * A parser for tables written to text files.
 *
 * @param input The input to this parser.
 */
class DocumentParser(val input: ParserInput) extends Parser {

  //
  // Entry point.
  //

  def Document: Rule1[model.Document] = rule {
    Whitespace ~ Title ~ Equals ~ (Boolean | Number | String | Table) ~ EOI ~> { (t: String, n: Node) =>
      model.Document(t, n)
    }
  }

  //
  // Composite rules.
  //

  //def Table: Rule1[Table] = LeftBracket ~ zeroOrMore {
  //  ???
  //} ~ RightBracket

  def Key: Rule1[Value] = rule {
    LeftBracket ~ (Boolean | Number | String) ~ RightBracket
  }

  def Entry: Rule1[(Value, Node)] = rule {
    (Key ~ Equals ~ (Boolean | Number | String | Table)) ~> { (k: Value, n: Node) => k -> n }
  }

  def Entries: Rule1[Seq[(Value, Node)]] = rule {
    Entry ~ zeroOrMore(Comma ~ Entry) ~ optional(Comma) ~> { (h: (Value, Node), t: Seq[(Value, Node)]) =>
      h +: t
    }
  }

  def Table: Rule1[model.Table] = rule {
    LeftBrace ~ optional(Entries) ~ RightBrace ~> { t: Option[Seq[(Value, Node)]] =>
      model.Table(ListMap((t getOrElse Seq.empty): _*))
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

  def Boolean: Rule1[Value.Boolean] = rule {
    capture(atomic("true" | "false")) ~> { (v: String) =>
      Value.Boolean(v.toBoolean)
    } ~ Whitespace
  }

  def Number: Rule1[Value.Number] = rule {
    capture(atomic {
      optional('-') ~ oneOrMore(CharPredicate.Digit) ~ optional(ch('.') ~ oneOrMore(CharPredicate.Digit))
    }) ~> { (v: String) =>
      Value.Number(java.lang.Double.parseDouble(v))
    } ~ Whitespace
  }

  def String: Rule1[Value.String] = rule {
    ch('"') ~ capture(zeroOrMore(noneOf("\"\\") | ("\\" ~ "\"") | ("\\" ~ "\\"))) ~ '"' ~> { (v: String) =>
      Value.String(v.replaceAll("""\"""", """"""").replaceAll("""\\""", """\"""))
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
