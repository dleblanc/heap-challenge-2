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

  def splitSelectorByTokens(selector: String): Seq[String] = {

    val matcher = tokenAndOptionalCombinatorRegex.pattern.matcher(selector)

    // Match a "selector with optional combinator" below
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

  def parseSelectorTokens(tokens: Seq[String]): Seq[SelectorNode] = {

    tokens
      .map {
        // NOTE: could use extractors here too

        case i if i.startsWith("#") => Id(i.replace("#",""))

        case i if i.startsWith(".") => WithClass(i.replace(".", ""))

        case ">" => Parent()

        case i => Tag(i)
      }
  }

  def countMatches(selector: String, dom: JsValue): Int = {

    val parsedSelector = parseSelectorTokens(splitSelectorByTokens(selector))

//    // Special case if we're given an "html/body" DOM.
//    if ((dom \ "tag").as[String] == "html") {
//      (dom \ "children" \ "children"
//    }

    recurComparison(parsedSelector, dom)
  }

  def recurComparison(selectors: Seq[SelectorNode], tree: JsValue): Int = {

    if (selectors.isEmpty) {
      return 1 // TODO: do in the pattern match below
    }

    // Return the # of selectors that matched
    selectors.head match {

//      case Nil => 1 // Return 1 if we've exhausted all our selectors??

      case  Tag(name) =>

        val (thisValue, searchSelectors) = if (name == (tree \ "tag").as[String]) {
          (if (selectors.tail.isEmpty) 1 else 0, selectors.tail)
        } else {
          (0, selectors)
        }

        thisValue + (tree \ "children")
          .toOption
          .flatMap { children => Option(children.as[Seq[JsValue]].map(recurComparison(searchSelectors, _))) }
          .getOrElse(Nil)
          .sum


      case Id(id) =>

        val (thisValue, searchSelectors) = if ((tree \ "id").toOption.exists(_.as[String] == id)) {
          (if (selectors.tail.isEmpty) 1 else 0, selectors.tail)
        } else {
          (0, selectors)
        }

        (tree \ "children")
          .toOption
          .flatMap { children => Option(children.as[Seq[JsValue]].map(recurComparison(searchSelectors, _))) }
          .getOrElse(Nil)
          .sum

//      case _ => -1
    }
  }

  test("SplitByTokens should return only the parent combinator when surrounded with spaces") {
    splitSelectorByTokens("a > b") should equal (Seq("a", ">", "b"))
  }

  test("SplitByTokens handles simple token") {
    splitSelectorByTokens("a") should equal (Seq("a"))
  }

  test("SplitByTokens handles hash sign") {
    splitSelectorByTokens("a #b") should equal (Seq("a", "#b"))
  }

  test("SplitByTokens handles dot in selector") {
    splitSelectorByTokens("a .b") should equal (Seq("a", ".b"))
  }

  test("parse tokens converts to expected objects") {

    parseSelectorTokens(Seq("a", "#b", ">", ".c")) should equal (Seq(Tag("a"), Id("b"), Parent(), WithClass("c")))
  }


  test("First test") {

    val firstJson = Json.parse(firstInput)

    val dom = (firstJson \ "hierarchy").get

    val opt = (dom \ "children")
      .toOption
        .map(_.as[Seq[JsValue]])

    opt
      .foreach(println _)


    val testSelectors: Seq[String] = (firstJson \ "tests").as[Seq[String]]

    val results = testSelectors.map {selector =>
      countMatches(selector, dom)
    }

    results should equal (List(2,2,2))

    // expect: [2,2,2]
  }

  test("Narrow in on first test") {

    val firstJson = Json.parse(firstInput)

    val dom = (firstJson \ "hierarchy").get

    countMatches("#content p", dom) should equal (2)

  }

}