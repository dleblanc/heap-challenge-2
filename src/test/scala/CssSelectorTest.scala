import org.scalatest._
import play.api.libs.json._

import scala.collection.mutable

class CssSelectorTest extends FunSuite with ShouldMatchers {

  private[this] val tokenAndOptionalCombinatorRegex =  """([#.\w]+)(\s+>?\s*)?""".r

  private[this] val firstInput =
    """
      |{
      |  "hierarchy": {
      |    "tag": "html",
      |    "children": [
      |      {
      |        "tag": "body",
      |        "classes": [
      |          "mobile"
      |        ],
      |        "children": [
      |          {
      |            "tag": "div",
      |            "id": "content",
      |            "children": [
      |              {
      |                "tag": "p"
      |              },
      |              {
      |                "tag": "p"
      |              }
      |            ]
      |          }
      |        ]
      |      }
      |    ]
      |  },
      |  "tests": [
      |    "p",
      |    "div p",
      |    "#content p"
      |  ]
      |}
    """.stripMargin


  sealed trait SelectorNode

  final case class Tag(name: String) extends SelectorNode
  final case class Parent() extends SelectorNode
  final case class Id(id: String) extends SelectorNode
  final case class WithClass(className: String) extends SelectorNode

  def splitByTokens(selector: String): Seq[String] = {

    val matcher = tokenAndOptionalCombinatorRegex.pattern.matcher(selector)

    val tokens = mutable.MutableList[String]()
    while (matcher.find) {
      tokens += matcher.group(1)

      Option(matcher.group(2))
          .map(_.trim)
          .flatMap(Option(_).filter(!_.isEmpty))
          .foreach(tokens += _)
    }
    tokens
  }

  def countMatches(selector: String, dom: JsValue): Int = {

    val parsedSelector = splitByTokens(selector)
    0
  }

  test("SplitByTokens should return only the parent combinator when surrounded with spaces") {
    splitByTokens("a > b") should equal (Seq("a", ">", "b"))
  }

  test("SplitByTokens handles simple token") {
    splitByTokens("a") should equal (Seq("a"))
  }

  test("SplitByTokens handles hash sign") {
    splitByTokens("a #b") should equal (Seq("a", "#b"))
  }

  test("SplitByTokens handles dot in selector") {
    splitByTokens("a .b") should equal (Seq("a", ".b"))
  }


  def parseTokens(tokens: Seq[String]): Seq[SelectorNode] = {

    tokens
      .map {
        // NOTE: could use extractors here too

        case i if i.startsWith("#") => Id(i.replace("#",""))

        case i if i.startsWith(".") => WithClass(i.replace(".", ""))

        case ">" => Parent()

        case i => Tag(i)
      }
  }

  test("parse tokens converts to expected objects") {
//    parseTokens(Seq("a", "#b", ">", ".c")) should equal (Seq(Tag("a'")))

    parseTokens(Seq("a")) should equal (Seq(Tag("a")))

    parseTokens(Seq("a", "#b", ">", ".c")) should equal (Seq(Tag("a"), Id("b"), Parent(), WithClass("c")))
  }


  test("First test") {

    val firstJson = Json.parse(firstInput)

    val dom = (firstJson \ "hierarchy").get

    val testCases = (firstJson \ "tests")
      .as[JsArray]
      value

    println(testCases)

    val results = testCases.value.map(i => countMatches(i.toString(), dom))

    results should equal (List(2,2,2))

    // expect: [2,2,2]
  }
}