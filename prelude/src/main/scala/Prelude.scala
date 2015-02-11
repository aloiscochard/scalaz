package scalaz

trait Prelude {
  type =:=[A, B] = scala.Predef.=:=[A, B]
  type <:<[A, B] = scala.Predef.<:<[A, B]
  type ~[P[_, _], Q[_, _]] = P[Nothing, Nothing] =:= Q[Nothing, Nothing]
  type \/[A, B] = Either[A, B]

  def const[A, B](a: A): B => A = _ => a
  def id[A](x: A): A = x
  def implicitly[A](implicit a: A): A = a

  type Function[A, B] = scala.Predef.Function[A, B]
}

object Prelude extends Prelude
