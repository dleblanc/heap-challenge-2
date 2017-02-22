import org.scalatest._

class SomethingTest extends FunSuite with ShouldMatchers {

  test("Test something") {
    1 should equal(1)
  }
}