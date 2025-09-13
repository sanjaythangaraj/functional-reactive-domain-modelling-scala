package chap6
package domain

import scalaz._
import Scalaz._

import scala.concurrent.Future

package object service {
  type Valid[A] = EitherT[Future, NonEmptyList[String], A]
}
