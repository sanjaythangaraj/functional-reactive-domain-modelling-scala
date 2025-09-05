package chap4.monoids_foldable.foldable

trait Foldable[F[_]] {
  def foldl[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(implicit m: Monoid[B]): B =
    foldl(as)(m.zero) { (b, a) => m.op(b, f(a)) }
}

object Foldable {
  implicit val listFoldable = new Foldable[List] {
    override def foldl[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  }
}

trait Utils {
  def mapReduce[F[_], A, B](as: F[A])(f: A => B)(implicit fd: Foldable[F], m: Monoid[B]): B =
    fd.foldMap(as)(f)
}