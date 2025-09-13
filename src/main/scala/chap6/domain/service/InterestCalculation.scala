package chap6
package domain
package service

import scalaz._
import Scalaz._

trait InterestCalculation[Account, Amount] {
  def computeInterest: Kleisli[Valid, Account, Amount]
}
