package chap4.patterns

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = ???
  def map[A, B](ma: F[A])(f: A => B): F[B] = {
    flatMap(ma)(a => unit(f(a)))
  }
  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = {
    flatMap(ma)(a => map(mb)(b => f(a, b)))
  }

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = {
    la.foldRight(unit(List[B]())) { (a, mlb) =>
      map2(f(a), mlb)(_ :: _)
    }
  }

  def sequence[A](lma: List[F[A]]): F[List[A]] = {
    traverse(lma)(identity)
  }

  def join[A](mma: F[F[A]]): F[A] = {
    flatMap(mma)(identity)
  }
}

object Monad {
  val optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)
    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma flatMap f
  }

  val listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma flatMap f
  }
}
