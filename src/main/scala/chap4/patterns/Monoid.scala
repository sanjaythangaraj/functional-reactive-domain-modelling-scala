package chap4.patterns

trait Monoid[T] {
  def zero: T
  def op(t1: T, t2: T): T
}

object Monoid {
  import common._

  def apply[T](implicit monoid: Monoid[T]): Monoid[T] = monoid

  implicit val bigDecimalAdditionMonoid = new Monoid[BigDecimal] {
    override def zero: BigDecimal = BigDecimal(0)
    override def op(t1: BigDecimal, t2: BigDecimal): BigDecimal = t1 + t2
  }

  implicit def mapMonoid[K, V: Monoid] = new Monoid[Map[K, V]] {
    override def zero: Map[K, V] = Map.empty[K, V]
    override def op(t1: Map[K, V], t2: Map[K, V]): Map[K, V] = t2.foldLeft(t1) {
      case (acc, (key, value)) =>
        acc.get(key)
          .map(v => acc + (key -> Monoid[V].op(value, v)))
          .getOrElse(acc + (key, value))
    }
  }

  final val zeroMoney: Money = Money(Monoid[Map[Currency, Amount]].zero)

  implicit def moneyAdditionMonoid = new Monoid[Money] {
    override def zero: Money = zeroMoney
    override def op(t1: Money, t2: Money): Money = Money(Monoid[Map[Currency, Amount]].op(t1.m, t2.m))
  }

  object MoneyOrdering extends Ordering[Money] {
    override def compare(x: Money, y: Money): Int = x.toBaseCurrency compare y.toBaseCurrency
  }

  import MoneyOrdering._

  implicit def moneyCompareMonoid = new Monoid[Money] {
    override def zero: Money = zeroMoney
    override def op(t1: Money, t2: Money): Money = if (gt(t1, t2)) t1  else t2
  }

  implicit def balanceAdditionMonoid = new Monoid[Balance] {
    implicit val m: Monoid[Money] = moneyAdditionMonoid
    override def zero: Balance = Balance()
    override def op(t1: Balance, t2: Balance): Balance = Balance(Monoid[Money].op(t1.amount, t2.amount))
  }
}