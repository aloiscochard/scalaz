package scalaz

abstract class Monad[M[_]] {
  implicit def bind: Bind[M]
  implicit def applicative: Applicative[M]

  implicit def apply: Apply[M] = applicative.apply
  implicit def functor: Functor[M] = applicative.apply.functor
}
