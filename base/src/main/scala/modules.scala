package scalaz
package modules

trait maybe
trait monad extends bind with applicative
trait traversable extends functor with foldable
  with TraversableSyntax

