
import org.scalatest._

class SumTest extends FunSuite with ShouldMatchers {

  test("Sum stuff") {

    val input = Array(1, 2, 5, 10)

    input.sum should equal (18)
  }

}
