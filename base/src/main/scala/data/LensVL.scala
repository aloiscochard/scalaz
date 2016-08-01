package scalaz
package data

import typeclass.Functor

final case class LensVL[S, T, A, B](get: S => A, set: S => B => T) {
  def apply[F[_]](afb: A => F[B])(implicit F: Functor[F]): S => F[T] = s => F.map(afb(get(s)))(set(s))

  /*
  def ∘[I, O](i: I)(implicit Compose: LensVL.Compose[I, O, S, T, A, B]): O = Compose(this)(i)
  def compose[I, O](i: I)(implicit Compose: LensVL.Compose[I, O, S, T, A, B]): O = Compose(this)(i)
  */
}

