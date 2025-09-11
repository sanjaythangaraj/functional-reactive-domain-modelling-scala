package chap5.domain.service

import scalaz.Kleisli

trait TaxCalculation[Amount] {
  def computeTax: Kleisli[Valid, Amount, Amount]
}
