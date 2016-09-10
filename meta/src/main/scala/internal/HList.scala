package scalaz
package internal

sealed trait HList

object HList {
  final case class HCons[H, XS <: HList](head: H, tail: XS) extends HList
  trait HNil extends HList
  val HNil: HNil = new HNil {}

  trait Generic[M, XS <: HList]

  object Generic {
    implicit def tuple3[X0, X1, X2] = new Generic[(X0, X1, X2), HCons[X2, HCons[X1, HCons[X0, HNil]]]] {}
  }
}


