package scalaz
package machines

sealed trait Step[K[_], O, R] {
  //def map[B](f: R => B): Step[K, O, B]
}

object Step {
  case class Stop[K[_], O, R]() extends Step[K, O, R] {
    //def map[B](f: R => B): Step[K, O, B] = Stop[K, O, B]
  }

  case class Yield[K[_], O, R](o: O, r: R) extends Step[K, O, R] {
    //def map[B](f: R => B): Step[K, O, B] = Yield[K, O, B](o, f(r))
  }

  abstract class Await[K[_], O, R] extends Step[K, O, R] { self =>
    def g[T]: T => R
    def kg[T]: K[T]
    def r: R
    //def map[B](f: R => B): Step[K, O, B]
  }
}
