package chap4.patterns

trait Functor[F[_]] {
  def map[A, B](a: F[A])(f: A  => B): F[B]
}

object Functor {
  def apply[F[_] : Functor]: Functor[F] =
    implicitly[Functor[F]]

  implicit def listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](a: List[A])(f: A => B): List[B] = a map f
  }

  implicit def optionFunctor: Functor[Option] = new Functor[Option] {
    override def map[A, B](a: Option[A])(f: A => B): Option[B] = a map f
  }

  implicit def Tuple2Functor[A1]: Functor[(A1, ?)] = new Functor[(A1, ?)] {
    override def map[A, B](a: (A1, A))(f: A => B): (A1, B) = (a._1, f(a._2))
  }

}


