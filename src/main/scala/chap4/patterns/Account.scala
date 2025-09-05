package chap4.patterns

import java.util.{Date, Calendar}

sealed trait Currency
case object USD extends Currency
case object JPY extends Currency
case object AUD extends Currency
case object INR extends Currency

object common {
  type Amount = BigDecimal
  val today = Calendar.getInstance.getTime
  val toBaseCurrencyMap: Map[Currency, Amount] =
    Map(USD -> 1, JPY -> 0.0067941838, AUD -> 0.65528394, INR -> 0.011342574)
}

import common._

import java.util.{Calendar, Date}

case class Money(m: Map[Currency, Amount]) {
  def toBaseCurrency: Amount = m.foldLeft(BigDecimal(0)) {
    case (total, (currency, amount)) =>
      total + toBaseCurrencyMap(currency) * amount
  }
}

case class Balance(amount: Money = Monoid.zeroMoney)

sealed trait Account {
  def no: String
  def name: String
  def dateOfOpen: Option[Date]
  def dateOfClose: Option[Date]
  def balance: Balance
}

final case class CheckingAccount(
    no: String,
    name: String,
    dateOfOpen: Option[Date],
    dateOfClose: Option[Date] = None,
    balance: Balance = Balance()
) extends Account

final case class SavingsAccount(
    no: String,
    name: String,
    rateOfInterest: Amount,
    dateOfOpen: Option[Date],
    dateOfClose: Option[Date] = None,
    balance: Balance = Balance()
) extends Account

object Account {

  /*
    Uses Applicative instance of ValidationNel which accumulates errors using Semigroup
   */

  object FailSlowApplicative {
    import scalaz._
    import syntax.apply._, syntax.std.option._, syntax.validation._

    private def validateAccountNo(no: String): ValidationNel[String, String] = {
      if (no.isEmpty || no.length < 5)
        s"Account No has to be at least 5 characters long: found $no"
          .failureNel[String]
      else no.successNel[String]
    }

    private def validateOpenCloseDate(
        od: Date,
        cd: Option[Date]
    ): ValidationNel[String, (Option[Date], Option[Date])] = {
      cd.map { c =>
        if (c before od)
          s"Close date [$c] cannot be earlier than open date [$od]"
            .failureNel[(Option[Date], Option[Date])]
        else (od.some, cd).successNel[String]
      }.getOrElse((od.some, cd).successNel[String])
    }

    private def validateRate(
        rate: BigDecimal
    ): ValidationNel[String, Amount] = {
      if (rate <= BigDecimal(0))
        s"Interest rate $rate must be > 0".failureNel[BigDecimal]
      else rate.successNel[String]
    }

    def checkingAccount(
        no: String,
        name: String,
        openDate: Option[Date],
        closeDate: Option[Date],
        balance: Balance
    ): ValidationNel[String, Account] = {
      (
        validateAccountNo(no) |@|
          validateOpenCloseDate(openDate.getOrElse(today), closeDate)
      ) { (n, d) =>
        CheckingAccount(n, name, d._1, d._2, balance)
      }
    }

    def savingsAccount(
        no: String,
        name: String,
        rate: BigDecimal,
        openDate: Option[Date],
        closeDate: Option[Date],
        balance: Balance
    ): ValidationNel[String, Account] = {
      (
        validateAccountNo(no) |@|
          validateOpenCloseDate(openDate.getOrElse(today), closeDate) |@|
          validateRate(rate)
      ) { (n, d, r) =>
        SavingsAccount(n, name, r, d._1, d._2, balance)
      }
    }
  }

  /*
    Uses default Applicative instance of Either which does not accumulate errors.
    But true to applicative nature it will invoke validations for all the items.
    So not truly fail fast - fail slow but non-accumulating
   */

  object FailFastApplicative {
    import scalaz._
    import syntax.apply._, syntax.std.option._, std.either._

    private def validateAccountNo(no: String): Either[String, String] = {
      if (no.isEmpty || no.length < 5)
        Left(s"Account No has to be at least 5 characters long: found $no")
      else Right(no)
    }

    private def validateOpenCloseDate(
        od: Date,
        cd: Date
    ): Either[String, (Date, Date)] = {
      if (cd before od)
        Left(s"Close date [$cd] cannot be earlier than open date [$od]")
      else Right((od, cd))
    }

    private def validateRate(rate: BigDecimal): Either[String, Amount] = {
      if (rate <= BigDecimal(0)) Left(s"Interest rate $rate must be > 0")
      else Right(rate)
    }

    def checkingAccount(
        no: String,
        name: String,
        openDate: Option[Date],
        closeDate: Option[Date],
        balance: Balance
    ): Either[String, Account] = {
      (validateAccountNo(no) |@|
        validateOpenCloseDate(
          openDate.getOrElse(today),
          closeDate.getOrElse(today)
        )) { (n, d) =>
        CheckingAccount(n, name, d._1.some, d._2.some, balance)
      }
    }

    def savingsAccount(
        no: String,
        name: String,
        rate: BigDecimal,
        openDate: Option[Date],
        closeDate: Option[Date],
        balance: Balance
    ): Either[String, Account] = {
      (
        validateAccountNo(no) |@|
          validateOpenCloseDate(
            openDate.getOrElse(today),
            closeDate.getOrElse(today)
          ) |@|
          validateRate(rate)
      ) { (n, d, r) =>
        SavingsAccount(n, name, r, d._1.some, d._2.some, balance)
      }
    }
  }

  /*
    Uses monad instance of Either which does not accumulate errors. True fail-fast.
   */

  object FailFastMonad {
    import scalaz._
    import syntax.std.option._

    private def validateAccountNo(no: String): Either[String, String] = {
      if (no.isEmpty || no.length < 5)
        Left(s"Account No has to be at least 5 characters long: found $no")
      else Right(no)
    }

    private def validateOpenCloseDate(
        od: Date,
        cd: Date
    ): Either[String, (Date, Date)] = {
      if (cd before od)
        Left(s"Close date [$cd] cannot be earlier than open date [$od]")
      else Right((od, cd))
    }

    private def validateRate(rate: BigDecimal): Either[String, Amount] = {
      if (rate <= BigDecimal(0)) Left(s"Interest rate $rate must be > 0")
      else Right(rate)
    }

    private def checkingAccount(
        no: String,
        name: String,
        openDate: Option[Date],
        closeDate: Option[Date],
        balance: Balance
    ): Either[String, Account] = {
      for {
        n <- validateAccountNo(no).right
        d <- validateOpenCloseDate(
          openDate.getOrElse(today),
          closeDate.getOrElse(today)
        ).right
      } yield CheckingAccount(n, name, d._1.some, d._2.some, balance)
    }

    def savingsAccount(
        no: String,
        name: String,
        rate: BigDecimal,
        openDate: Option[Date],
        closeDate: Option[Date],
        balance: Balance
    ): Either[String, Account] = {
      for {
        n <- validateAccountNo(no).right
        d <- validateOpenCloseDate(
          openDate.getOrElse(today),
          closeDate.getOrElse(today)
        ).right
        r <- validateRate(rate).right
      } yield SavingsAccount(n, name, r, d._1.some, d._2.some, balance)
    }
  }

}


