package scalaz

abstract class Applicative[F[_]] {
  implicit def apply: Apply[F]
  def pure[A](a: A): F[A]

  implicit def functor: Functor[F] = apply.functor
}

object Applicative {
  trait Syntax {
    implicit class AnyToApplicativeOps[A](a: A) {
      def pure[F[_]](implicit F: Applicative[F]): F[A] = F.pure(a)
    }
  }
}
