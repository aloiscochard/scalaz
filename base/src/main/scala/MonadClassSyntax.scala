package scalaz
package typeclass

import scala.language.implicitConversions

trait MonadClassSyntax {
  implicit def MonadApplicativeApplyFunctorOps[F[_], A](fa: F[A])(implicit F: Functor[F]): FunctorSyntax.Ops[F, A] =
    new FunctorSyntax.Ops(fa)

  implicit def MonadApplicativeApplyOps[F[_], A](fa: F[A])(implicit F: Apply[F]): ApplySyntax.Ops[F, A] =
    new ApplySyntax.Ops(fa)

  implicit def MonadApplicativeOpsA[A](a: A): ApplicativeSyntax.OpsA[A] = new ApplicativeSyntax.OpsA(a)
}

