package chap4.patterns

import scala.language.higherKinds
import java.util.{Date, Calendar}

object Loans {
  import scalaz._
  import Scalaz._

  val today = Calendar.getInstance.getTime

  case class LoanApplication[Status] private[Loans] (
      date: Date,
      name: String,
      purpose: String,
      repayIn: Int,
      actualRepaymentYears: Option[Int] = None,
      startDate: Option[Date] = None,
      loanNo: Option[String] = None,
      emi: Option[BigDecimal] = None
  )

  trait Applied
  trait Approved
  trait Enriched
  trait Disbursed

  type LoanApplied = LoanApplication[Applied]
  type LoanApproved = LoanApplication[Approved]
  type LoanEnriched = LoanApplication[Enriched]
  type LoanDisbursed = LoanApplication[Disbursed]

  def applyLoan(
      name: String,
      purpose: String,
      repayIn: Int,
      date: Date = today
  ): LoanApplied =
    LoanApplication[Applied](date, name, purpose, repayIn)

  def approve: Kleisli[Option, LoanApplied, LoanApproved] =
    Kleisli[Option, LoanApplied, LoanApproved] { l =>
      Some(
        LoanApplication[Approved](
          date = l.date,
          name = l.name,
          purpose = l.purpose,
          repayIn = l.repayIn,
          actualRepaymentYears = Some(15),
          startDate = Some(today),
          loanNo = Some(scala.util.Random.nextString(10)),
          emi = l.emi
        )
      )
    }

  def enrich: Kleisli[Option, LoanApproved, LoanEnriched] =
    Kleisli[Option, LoanApproved, LoanEnriched] { l =>
      val x = for {
        y <- l.actualRepaymentYears
        s <- l.startDate
      } yield (y, s)

      Some(
        LoanApplication[Enriched](
          date = l.date,
          name = l.name,
          purpose = l.purpose,
          repayIn = l.repayIn,
          actualRepaymentYears = l.actualRepaymentYears,
          startDate = l.startDate,
          loanNo = l.loanNo,
          emi = x.map {
            case (y, s) => calculateEMI(y, s)
          }
        )
      )
    }

  private def calculateEMI(tenure: Int, startDate: Date): BigDecimal = BigDecimal(0)

  val l = applyLoan("george", "house building", 10)
  val op = approve andThen enrich

  op run l
}
