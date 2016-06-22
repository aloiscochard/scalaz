package scalaz
package typeclass

import scala.language.implicitConversions

trait ApplicativeClassSyntax extends ApplicativeSyntax {
  implicit def ApplicativeApplyFunctorOps[F[_], A](fa: F[A])(implicit F: Functor[F]): FunctorSyntax.Ops[F, A] =
    new FunctorSyntax.Ops(fa)

  implicit def ApplicativeApplyOps[F[_], A](fa: F[A])(implicit F: Apply[F]): ApplySyntax.Ops[F, A] =
    new ApplySyntax.Ops(fa)
}

