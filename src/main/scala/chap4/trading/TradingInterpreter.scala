package chap4.trading

import java.util.{Calendar, Date}
import scalaz.{Order => OrderZ, _}
import Scalaz._
import Kleisli._
import TradeModel._

trait TradingInterpreter
    extends Trading[Account, Trade, ClientOrder, Order, Execution, Market] {

  override def clientOrders: ReaderT[List, List[ClientOrder], Order] = Kleisli(
    fromClientOrders
  )
  override def execute(
      market: Market,
      brokerAccount: Account
  ): Kleisli[List, Order, Execution] =
    Kleisli { order =>
      order.items.map { item =>
        Execution(
          brokerAccount,
          item.ins,
          "e-123",
          market,
          item.price,
          item.qty
        )
      }
    }

  override def allocate(
      accounts: List[Account]
  ): Kleisli[List, Execution, Trade] =
    Kleisli { execution =>
      val q = execution.quantity / accounts.size
      accounts.map { account =>
        makeTrade(
          account,
          execution.instrument,
          "t-123",
          execution.market,
          execution.unitPrice,
          q
        )
      }
    }
}

object TradingInterpreter extends TradingInterpreter
