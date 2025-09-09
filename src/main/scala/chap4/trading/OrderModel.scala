package chap4.trading

import java.util.Date

trait OrderModel { this: RefModel =>
  case class LineItem(ins: Instrument, qty: BigDecimal, price: BigDecimal)
  case class Order(no: String, date: Date,customer: Customer, items: List[LineItem])

  type ClientOrder = Map[String, String]

  def fromClientOrders: List[ClientOrder] => List[Order] = { cos =>
    cos map { co =>
      val ins: Array[String] = co("instrument").split("-")
      val lineItems = ins map {in =>
        var arr: Array[String] = in.split("/")
        LineItem(arr(0), BigDecimal(arr(1)), BigDecimal(arr(2)))
      }
      Order(co("no"), today(), co("customer"), lineItems.toList)
    }
  }
}