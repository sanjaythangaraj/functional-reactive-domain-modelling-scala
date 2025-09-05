package chap4.patterns

import org.scalatest.{FunSuite, Matchers}

class FunctorTest extends FunSuite with Matchers{
  import chap4.patterns.Functor._

  val list = List(1,2,3,4)
  val f: Int => Int = _ + 1
  val tupleList = List(("a", 10), ("b", 20))

  test ("Functor[List] map") {
    Functor[List].map(list)(f) shouldBe List(2,3,4,5)
  }

  test("Functor[Tuple2]] map") {
    Functor[List].map(tupleList) { tuple =>
      Functor[(String, ?)].map(tuple)(f)
    } shouldBe List(("a", 11), ("b", 21))
  }

}