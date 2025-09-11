package chap6.playground

import scalaz._
import Scalaz._

object App extends App {

  val count: String \/ Option[Int] = some(10).right[String]
  val count2: String \/ Option[Int] = None.right
  val count3: String \/ Option[Int] = "error".left

  val a: String \/ Option[Int] = for {
    maybeCount <- count
  } yield {
    for {
      c <- maybeCount
    } yield c
  }

  println(a) //  \/-(Some(10))

  val b: String \/ Option[Int] = for {
    maybeCount <- count2
  } yield {
    for {
      c <- maybeCount
    } yield c
  }

  println(b) // \/-(None)

  val c: String \/ Option[Int] = for {
    maybeCount <- count3
  } yield {
    for {
      c <- maybeCount
    } yield c
  }

  println(c) // -\/(error)

  /* Using OptionT */

  type Error[A] = String \/ A
  type Response[A] = OptionT[Error, A]

  val count4: Response[Int] = OptionT {
    val t: String \/ Option[Int] = 10.some.right[String]
    t
  }

  val d = for {
    c <- count4
  } yield c

  println(d) // OptionT(\/-(Some(10)))

  val count5: Response[Int] = OptionT {
    "error".left[Option[Int]]
  }

  val e = for {
    c <- count5
  } yield c

  println(e) // OptionT(-\/(error))
}
