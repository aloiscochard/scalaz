package scalaz
package typeclass

trait Monad[M[_]] {
  def applicative: Applicative[M]
  def bind: Bind[M]
}

object Monad extends MonadInstances with FunctorSyntax {
  def apply[M[_]](implicit M: Monad[M]): Monad[M] = M

  object Class extends MonadClassSyntax
}

