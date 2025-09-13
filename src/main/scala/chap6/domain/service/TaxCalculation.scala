package chap6
package domain
package service

import scalaz.Kleisli

trait TaxCalculation[Amount] {
  def computeTax: Kleisli[Valid, Amount, Amount]
}
