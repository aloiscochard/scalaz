package scalaz
package data

import scala.language.experimental.macros

import typeclass.Functor

trait LensFunctions {
  /*
  def lens[S, T, A, B](sa: S => A)(sbt: S => B => T): Lens[S, T, A, B] = new Lens[S, T, A, B] {
    def get(s: S): A = sa(s)
    def set(s: S, b: B): T = sbt(s)(b)
  }
  def slens[S, A](sa: S => A)(sas: S => A => S): Lens[S, S, A, A] = lens[S, S, A, A](sa)(sas)
  */

  def slens[S, A](sa: S => A)(sas: S => A => S): Lens[S, S, A, A] =
    macro LensMacros.slens[S, A]
}
