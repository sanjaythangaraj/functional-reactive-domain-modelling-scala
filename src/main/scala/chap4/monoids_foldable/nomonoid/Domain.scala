package chap4.monoids_foldable.nomonoid

sealed trait TransactionType
case object DR extends TransactionType
case object CR extends TransactionType

sealed trait Currency
case object USD extends Currency
case object JPY extends Currency
case object AUD extends Currency
case object INR extends Currency

object common {
  type Amount = BigDecimal
  val toBaseCurrencyMap: Map[Currency, Amount] =
    Map(USD -> 1, JPY -> 0.0067941838, AUD -> 0.65528394, INR -> 0.011342574)
}

import common._

import java.util.Date

case class Money(m: Map[Currency, Amount]) {
  def toBaseCurrency: Amount = m.foldLeft(BigDecimal(0)) {
    case (total, (currency, amount)) =>
      total + toBaseCurrencyMap(currency) * amount
  }

  def +(that: Money) = {
    Money {
      that.m.foldLeft(m) {
        case (acc, (currency, amount)) =>
          acc.get(currency).map { amt =>
            acc + (currency -> (amount + amt))
          }.getOrElse(acc + (currency -> amount))
      }
    }
  }
}

object Money {
  val zero = Money(Map.empty[Currency, BigDecimal])
}

object MoneyOrdering extends Ordering[Money] {
  override def compare(x: Money, y: Money): Int =
    x.toBaseCurrency compare y.toBaseCurrency
}

case class Transaction(
    txid: String,
    accountNo: String,
    date: Date,
    amount: Money,
    txnType: TransactionType,
    status: Boolean
)

case class Balance(b: Money)

trait Analytics[Transaction, Balance, Money] {
  def maxDebitOnDay(txns: List[Transaction]): Money
  def sumBalances(bs: List[Balance]): Money
}

object Analytics extends Analytics[Transaction, Balance, Money] {

  import MoneyOrdering._

  override def maxDebitOnDay(txns: List[Transaction]): Money = {
    txns
      .filter(_.txnType == DR)
      .foldLeft(Money.zero) { (acc, txn) =>
        if (gt(txn.amount, acc)) txn.amount else acc
      }
  }

  override def sumBalances(bs: List[Balance]): Money =
    bs.foldLeft(Money.zero)(_ + _.b)
}
