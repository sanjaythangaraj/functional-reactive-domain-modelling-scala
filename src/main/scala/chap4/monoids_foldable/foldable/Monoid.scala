package chap4.monoids_foldable.foldable

trait Monoid[T] {
  def zero: T
  def op(t1: T, t2: T): T
}

object Monoid {
  def apply[T](implicit monoid: Monoid[T]) = monoid

  implicit val BigDecimalAdditionMonoid = new Monoid[BigDecimal] {
    override def zero: BigDecimal = BigDecimal(0)
    override def op(t1: BigDecimal, t2: BigDecimal): BigDecimal = t1 + t2
  }

  implicit def MapMonoid[K, V: Monoid] = new Monoid[Map[K, V]] {
    override def zero: Map[K, V] = Map.empty[K, V]
    override def op(t1: Map[K, V], t2: Map[K, V]): Map[K, V] = t2.foldLeft(t1) {
      case (acc, (key, value)) =>
        acc.get(key).map { v =>
          acc + (key -> Monoid[V].op(value, v))
        }.getOrElse(acc + (key -> value))
    }
  }

  final val zeroMoney: Money = Money(Monoid[Map[Currency, BigDecimal]].zero)

  implicit def MoneyAdditionMonoid = new Monoid[Money] {
    override def zero: Money = zeroMoney
    override def op(t1: Money, t2: Money): Money = Money(Monoid[Map[Currency, BigDecimal]].op(t1.m, t2.m))
  }

  object MoneyOrdering extends Ordering[Money] {
    override def compare(x: Money, y: Money): Int = x.toBaseCurrency compare y.toBaseCurrency
  }

  import MoneyOrdering._

  implicit val MoneyCompareMonoid = new Monoid[Money] {
    override def zero: Money = zeroMoney
    override def op(t1: Money, t2: Money): Money = if (gt(t1, t2)) t1 else t2
  }

}
