package chap4.monoids_foldable.foldable

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
}

trait Analytics[Transaction, Balance, Money] {
  def maxDebitOnDay(txns: List[Transaction])(implicit m: Monoid[Money]): Money
  def sumBalances(bs: List[Balance])(implicit m: Monoid[Money]): Money
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

object Analytics extends Analytics[Transaction, Balance, Money] with Utils {

  override def maxDebitOnDay(
      txns: List[Transaction]
  )(implicit m: Monoid[Money]): Money = {
    mapReduce(txns.filter(_.txnType == DR))(_.amount)
  }
  override def sumBalances(
      bs: List[Balance]
  )(implicit m: Monoid[Money]): Money = {
    mapReduce(bs)(_.b)
  }
}
