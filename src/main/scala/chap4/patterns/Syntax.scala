package chap4.patterns

object Syntax {
  implicit class FunctorSyntax[F[_]: Functor, A](a: F[A]) {
    def map[B](f: A => B): F[B] = Functor[F].map(a)(f)
  }
}