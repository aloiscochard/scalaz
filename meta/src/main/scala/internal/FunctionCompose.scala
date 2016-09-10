package scalaz
package internal

import HList._

trait FunctionCompose[XS <: HList, A, B]

object FunctionCompose {
  implicit def fun3[A, B, C, D] = new FunctionCompose[HCons[C => D, HCons[B => C, HCons[A => B, HNil]]], A, C] { }
}

trait FunctionSyntax {
  implicit class FunctionComposeOps[M, XS <: HList](m: M)(implicit XS: HList.Generic[M, XS]) {
    def compose[A, B](a: A)(implicit XSAB: FunctionCompose[XS, A, B]): B = ???
    def compose_[A, B](implicit XSAB: FunctionCompose[XS, A, B]): A => B = ???
  }

  implicit class FunctionOps[A, B](val f: A => B) {
    def ∘[C](g: B => C): A => C = a => g(f(a))
  }
}

object FunctionSyntax extends FunctionSyntax

object Demo {
  import FunctionSyntax._

  val f: Int => String = _.toString
  val g: String => List[String] = s => s.split(" ").toList
  val h: List[String] => Int = _.size

  (f ∘ g ∘ h)(42)
  (f, g, h).compose(42)
  (f, g, h).compose_
}
