package scalaz
package data

import scala.annotation.tailrec
import scala.language.experimental.macros

import data.HList._

/** A telephoto lens provide optimized optics composition. **/
final class Telephoto[OL <: HList, A](val optics: OL) extends AnyVal {
  def optic: A = macro TelephotoMacros.composeHList[OL, A]
}

object Telephoto {
  def apply[OL <: HList](optics: OL)(implicit OL: Focal[OL]): Telephoto[OL, OL.O] = new Telephoto[OL, OL.O](optics)

  def compose[
    O0, O1, O2, O3, O4, O5, O6, O7, O8, O9,
    O
  ](
    o0: O0, o1: O1, o2: O2, o3: O3, o4: O4, o5: O5, o6: O6, o7: O7, o8: O8, o9: O9
  )(implicit OL: Focal.Aux[
    HCons[O9, HCons[O8, HCons[O7, HCons[O6, HCons[O5, HCons[O4, HCons[O3, HCons[O2, HCons[O1, HCons[O0, HNil]]]]]]]]]],
    O
  ]): O =
    macro TelephotoMacros.compose9[O0, O1, O2, O3, O4, O5, O6, O7, O8, O9, O, Focal[HCons[O9, HCons[O8, HCons[O7, HCons[O6, HCons[O5, HCons[O4, HCons[O3, HCons[O2, HCons[O1, HCons[O0, HNil]]]]]]]]]]]]

  /** A focal is a type level function which given a heterogenous list of optics,
   *  resolve to the optical type of their composition. **/
  sealed trait Focal[OL <: HList] {
    type O
  }

  object Focal {

    class Aux[OL <: HList, O]

    object Aux {
      implicit def aux[OL <: HList, O](implicit OL: Focal[OL]): Aux[OL, OL.O] = new Aux[OL, OL.O]
    }

    final class FLens[S, T, A, B, OL <: HList, SS, TT, AA, BB] extends Focal[HCons[Lens[S, T, A, B], OL]] {
      type O = Lens[SS, TT, AA, BB]
    }

    implicit def focal0[S, T, A, B] =
      new FLens[S, T, A, B, HNil, S, T, A, B]

    implicit def focal3[S, T, A, B, C, D, OL <: HList, SS, TT, AA, BB](implicit Focal: FLens[S, T, A, B, OL, SS, TT, AA, BB]) =
      new FLens[A, B, C, D, HCons[Lens[S, T, A, B], OL], SS, TT, C, D]
  }

}
