package scalaz
package typeclass

package modules {
  trait monad extends bind with applicative
  type traversable = Traversable.Syntax
}
