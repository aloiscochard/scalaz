package scalaz

abstract class Bind[F[_]] {
  def bind[A, B](f: A => F[B]): F[A] => F[B]
}

object Bind {
  trait Syntax {
    implicit class BindOps[F[_], A](fa: F[A])(implicit F: Bind[F]) {
      def flatMap[B](f: A => F[B]): F[B] = F.bind(f)(fa)
    }
  }
}
