import org.scalatest._

class SampleTest extends FunSuite with ShouldMatchers {

  test("Test something") {
    val n = 6

    val result = (1 to n
      map { i =>
        " " * (n - i) + "#" * i
      }
    )
      .mkString("\n")


    println(result + "\n")
  }
}