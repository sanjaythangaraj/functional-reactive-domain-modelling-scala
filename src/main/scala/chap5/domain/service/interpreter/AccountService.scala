package chap5
package domain
package service
package interpreter

import java.util.Date
import scalaz._
import Scalaz._
import \/._
import Kleisli._

import model.{Account, Balance}
import model.common._
import repository.AccountRepository

class AccountServiceInterpreter
    extends AccountService[Account, Amount, Balance] {

  override def open(
      no: String,
      name: String,
      rate: Option[Amount],
      openingDate: Option[Date],
      accountType: AccountType
  ): AccountOperation[Account] = Kleisli[Valid, AccountRepository, Account] {
    (repo: AccountRepository) =>
      repo.query(no) match {
        case \/-(Some(a)) =>
          NonEmptyList(s"Already existing account with no $no").left[Account]
        case \/-(None) =>
          accountType match {
            case Checking =>
              Account
                .checkingAccount(no, name, openingDate, None, Balance())
                .flatMap(repo.store)
            case Savings =>
              rate map {
                Account
                  .savingsAccount(no, name, _, openingDate, None, Balance())
                  .flatMap(repo.store)
              } getOrElse {
                NonEmptyList(s"Rate needs to be given for savings account")
                  .left[Account]
              }
          }
        case a @ -\/(_) => a
      }
  }

  override def close(
      no: String,
      closeDate: Option[Date]
  ): AccountOperation[Account] = Kleisli[Valid, AccountRepository, Account] {
    (repo: AccountRepository) =>
      repo.query(no) match {
        case \/-(None) =>
          NonEmptyList(s"Account $no does not exist").left[Account]
        case \/-(Some(a)) =>
          val cd = closeDate.getOrElse(today)
          Account.close(a, cd).flatMap(repo.store)
        case a @ -\/(_) => a
      }
  }

  private trait DC
  private case object D extends DC
  private case object C extends DC

  private def up(
      no: String,
      amount: Amount,
      dc: DC
  ): AccountOperation[Account] = Kleisli[Valid, AccountRepository, Account] {
    (repo: AccountRepository) =>
      repo.query(no) match {
        case \/-(None) => NonEmptyList(s"Account $no does not exist").left[Account]
        case \/-(Some(a)) => dc match {
          case D => Account.updateBalance(a, -amount).flatMap(repo.store)
          case C => Account.updateBalance(a, amount).flatMap(repo.store)
        }
        case a @ -\/(_) => a
      }
  }

  override def debit(no: String, amount: Amount): AccountOperation[Account] = up(no, amount, D)
  override def credit(no: String, amount: Amount): AccountOperation[Account] = up(no, amount, C)

  override def balance(no: String): AccountOperation[Balance] =
    Kleisli[Valid, AccountRepository, Balance] { (repo: AccountRepository) =>
      repo.balance(no)
    }
}

object AccountService extends AccountServiceInterpreter
