package chap4.monoid_foldable.monoid

import chap4.monoids_foldable.monoid._

import org.scalatest.{FunSuite, Matchers}

class AnalyticsTest extends FunSuite with Matchers {
  test("Analytics correctly finds max debit on a day") {
    val date = new java.util.Date()
    val txns = List(
      Transaction("tx1", "acc1", date, Money(Map(USD -> 100)), DR, true),
      Transaction("tx2", "acc1", date, Money(Map(USD -> 200)), DR, true),
      Transaction("tx3", "acc1", date, Money(Map(USD -> 250)), CR, true),
      Transaction("tx4", "acc1", date, Money(Map(JPY -> 30000)), DR, true)
    )
    implicit val m: Monoid[Money] = Monoid.MoneyCompareMonoid
    val maxDebit = Analytics.maxDebitOnDay(txns)
    maxDebit.m shouldBe Map(JPY -> 30000)
  }

  test ("return zero for empty transaction list") {
    implicit val m: Monoid[Money] = Monoid.MoneyCompareMonoid
    val maxDebit = Analytics.maxDebitOnDay(Nil)
    maxDebit shouldBe Monoid.zeroMoney
  }

  test ("return zero for no debit transactions") {
    val date = new java.util.Date()
    val txns = List(
      Transaction("tx1", "acc1", date, Money(Map(USD -> 100)), CR, true)
    )
    implicit val m: Monoid[Money] = Monoid.MoneyCompareMonoid
    val maxDebit = Analytics.maxDebitOnDay(txns)
    maxDebit shouldBe Monoid.zeroMoney
  }

  test ("correctly sum balances") {
    val balances = List(
      Balance(Money(Map(USD -> 100, JPY -> 1000))),
      Balance(Money(Map(AUD -> 50, USD -> 50))),
      Balance(Money(Map(INR -> 200)))
    )

    implicit val m: Monoid[Money] = Monoid.MoneyAdditionMonoid
    val sum = Analytics.sumBalances(balances)
    sum.m shouldBe Map(
      USD -> 150,
      JPY -> 1000,
      AUD -> 50,
      INR -> 200
    )
  }

  test ("return zero for empty balance list") {
    implicit val m: Monoid[Money] = Monoid.MoneyAdditionMonoid
    val sum = Analytics.sumBalances(Nil)
    sum shouldBe Monoid.zeroMoney
  }
}
