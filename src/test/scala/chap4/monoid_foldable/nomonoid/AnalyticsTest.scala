package chap4.monoid_foldable.nomonoid

import chap4.monoids_foldable.nomonoid._
import chap4.monoids_foldable.nomonoid.common._
import org.scalatest.{FunSuite, Matchers}

class AnalyticsTest extends FunSuite with Matchers {
  test("Money correctly converts to base currency") {
    val money = Money(Map(USD -> 100, JPY -> 1000, AUD -> 50, INR -> 200))
    val expected = (100 * toBaseCurrencyMap(USD)) +
      (1000 * toBaseCurrencyMap(JPY)) +
      (50 * toBaseCurrencyMap(AUD)) +
      (200 * toBaseCurrencyMap(INR))

    money.toBaseCurrency shouldBe expected
  }

  test("+ correctly adds two Money instances") {
    val money1 = Money(Map(USD -> 50, JPY -> 1000))
    val money2 = Money(Map(JPY -> 500, AUD -> 20))
    val result = money1 + money2
    result.m shouldBe Map(USD -> 50, JPY -> 1500, AUD -> 20)
  }

  test(" MoneyOrdering correctly compares Money instances") {
    val money1 = Money(Map(USD -> 100))
    val money2 = Money(Map(USD -> 200))
    MoneyOrdering.compare(money1, money2) shouldBe -1
    MoneyOrdering.compare(money2, money1) shouldBe 1
  }

  test("Analytics correctly finds max debit on a day") {
    val date = new java.util.Date()
    val txns = List(
      Transaction("tx1", "acc1", date, Money(Map(USD -> 100)), DR, true),
      Transaction("tx2", "acc1", date, Money(Map(USD -> 200)), DR, true),
      Transaction("tx3", "acc1", date, Money(Map(USD -> 250)), CR, true),
      Transaction("tx4", "acc1", date, Money(Map(JPY -> 30000)), DR, true)
    )
    val maxDebit = Analytics.maxDebitOnDay(txns)
    maxDebit.m shouldBe Map(JPY -> 30000)
  }

  test ("return zero for empty transaction list") {
    val maxDebit = Analytics.maxDebitOnDay(Nil)
    maxDebit shouldBe Money.zero
  }

  test ("return zero for no debit transactions") {
    val date = new java.util.Date()
    val txns = List(
      Transaction("tx1", "acc1", date, Money(Map(USD -> 100)), CR, true)
    )
    val maxDebit = Analytics.maxDebitOnDay(txns)
    maxDebit shouldBe Money.zero
  }

  test ("correctly sum balances") {
    val balances = List(
      Balance(Money(Map(USD -> 100, JPY -> 1000))),
      Balance(Money(Map(AUD -> 50, USD -> 50))),
      Balance(Money(Map(INR -> 200)))
    )
    val sum = Analytics.sumBalances(balances)
    sum.m shouldBe Map(
      USD -> 150,
      JPY -> 1000,
      AUD -> 50,
      INR -> 200
    )
  }

  test ("return zero for empty balance list") {
    val sum = Analytics.sumBalances(Nil)
    sum shouldBe Money.zero
  }
}
