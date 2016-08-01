package scalaz
package data

sealed trait HList

object HList {
  final case class HCons[H, T <: HList](head: H, tail: T) extends HList
  case object HNil extends HList
}
