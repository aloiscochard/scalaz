package scalaz
package system

trait ApplicativeCore[F[_]] {
  def core_map[A, B](ma: F[A])(f: A => B): F[B]
  def core_ap[A, B](fa: F[A])(f: F[A => B]): F[B]
  def core_pure[A](a: A): F[A]
}
