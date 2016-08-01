package scalaz
package data

import typeclass.Liskov._

import Maybe.{Empty, Just, Lazy}

trait MaybeFunctions {
  def empty[A]: Maybe[A] = liftCvf[Nothing, A, Maybe](isa[Nothing, A]).apply(Empty)
  def just[A](a: A): Maybe[A] = Just(a)
  def lazyMaybe[A](a: => Maybe[A]): Maybe[A] = new Lazy(a)

  def maybe[A, B](n: B)(f: A => B): Maybe[A] => B = _.fold(f, n)

  def fromOption[A](oa: Option[A]): Maybe[A] = oa.fold[Maybe[A]](empty[A])(Just(_))
}
