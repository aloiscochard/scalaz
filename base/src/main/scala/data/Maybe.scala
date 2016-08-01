package scalaz
package data

sealed abstract class Maybe[A] {
  final def fold[B](f: A => B, b: => B): B = this match {
    case Maybe.Just(a)  => f(a)
    case Maybe.Empty    => b
    case _              => this.asInstanceOf[Maybe.Lazy[A]].run.fold(f,b)
  }
}

object Maybe extends MaybeFunctions with MaybeInstances with MaybeSyntax {
  final private[data] case object Empty extends Maybe[Nothing]
  final case class Just[A](a: A) extends Maybe[A]
  final private[data] class Lazy[A](a: => Maybe[A]) extends Maybe[A] {
    def run = a
  }
}
