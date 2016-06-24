package scalaz
package modules

import typeclass._
// Typeclasses

// TODO Add missing typeclasses

trait Apply  extends ApplySyntax
                with Functor {
  type Apply[F[_]] = typeclass.Apply[F]
}

trait Applicative extends ApplicativeSyntax {
  type Applicative[F[_]] = typeclass.Apply[F]
}


trait Bind extends BindSyntax
              with BindFunctions
              with Apply {
  type Bind[M[_]] = typeclass.Bind[M]
}


trait Functor  extends FunctorSyntax
                  with FunctorFunctions {
  type Functor[F[_]] = typeclass.Functor[F]
}

trait Monad  extends Applicative
                with Bind {
  type Monad[M[_]] = typeclass.Monad[M]
}

