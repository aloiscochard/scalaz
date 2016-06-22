package scalaz
package typeclass

import scala.language.implicitConversions

trait ApplyClassSyntax extends ApplySyntax {
  implicit def ApplyFunctorOps[F[_], A](fa: F[A])(implicit F: Functor[F]): FunctorSyntax.Ops[F, A] =
    new FunctorSyntax.Ops(fa)
}
