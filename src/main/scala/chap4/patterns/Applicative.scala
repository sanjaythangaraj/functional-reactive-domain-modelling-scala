package chap4.patterns

trait Applicative[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def ap[A, B](f: F[A => B])(a: F[A]): F[B]
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    ap(map(fa)(f.curried))(fb)
  }
  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]())) { (a, fbs) => map2(f(a), fbs)(_ :: _)}
  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(identity)
}

object Applicative {
  def apply[F[_]: Applicative]: Applicative[F] = implicitly[Applicative[F]]

  implicit def listApply: Applicative[List] = new Applicative[List] {
    override def unit[A](a: => A): List[A] = List(a)
    override def ap[A, B](fs: List[A => B])(as: List[A]): List[B] = for {
      a <- as
      f <- fs
    } yield f(a)
    override def map[A, B](a: List[A])(f: A => B): List[B] = a map f
  }

  implicit def optionApply: Applicative[Option] = new Applicative[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)
    override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = (ff, fa) match {
      case (Some(f), Some(a)) => Some(f(a))
      case _ => None
    }
    override def map[A, B](a: Option[A])(f: A => B): Option[B] = a map f
  }
}