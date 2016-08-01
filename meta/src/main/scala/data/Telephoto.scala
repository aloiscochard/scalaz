package scalaz
package data

import scala.annotation.tailrec
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

import data.HList._

/** A telephoto lens provide optimized optics composition. **/
final class Telephoto[OL <: HList, A](val optics: OL) extends AnyVal {
  def optic: A = macro Telephoto.Macros.composeHList[OL, A]
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
    macro Telephoto.Macros.compose9[O0, O1, O2, O3, O4, O5, O6, O7, O8, O9, O, Focal[HCons[O9, HCons[O8, HCons[O7, HCons[O6, HCons[O5, HCons[O4, HCons[O3, HCons[O2, HCons[O1, HCons[O0, HNil]]]]]]]]]]]]

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

  object Macros {
    def compose9[
      O0: c.WeakTypeTag, O1: c.WeakTypeTag, O2: c.WeakTypeTag, O3: c.WeakTypeTag, O4: c.WeakTypeTag,
      O5: c.WeakTypeTag, O6: c.WeakTypeTag, O7: c.WeakTypeTag, O8: c.WeakTypeTag, O9: c.WeakTypeTag,
      O: c.WeakTypeTag, OL: c.WeakTypeTag
    ](c: Context)(
      o0: c.Expr[O0], o1: c.Expr[O1], o2: c.Expr[O2], o3: c.Expr[O3], o4: c.Expr[O4],
      o5: c.Expr[O5], o6: c.Expr[O6], o7: c.Expr[O7], o8: c.Expr[O8], o9: c.Expr[O9]
    )(OL: c.Expr[OL]): c.Expr[O] = {
      import c.universe._
      import Flag._

      val os = List(o9.tree, o8.tree, o7.tree, o6.tree, o5.tree, o4.tree, o3.tree, o2.tree, o1.tree, o0.tree).toVector
      val xs = List(c.weakTypeOf[O9], c.weakTypeOf[O8], c.weakTypeOf[O7], c.weakTypeOf[O6], c.weakTypeOf[O5],
                    c.weakTypeOf[O4], c.weakTypeOf[O3], c.weakTypeOf[O2], c.weakTypeOf[O1], c.weakTypeOf[O0])

      _compose[O](c)(xs)(os(_))
    }

    def composeHList[OL <: HList: c.WeakTypeTag, O: c.WeakTypeTag](c: Context): c.Expr[O] = {
      import c.universe._
      import Flag._

      def hunfold(tpe: Type): List[Type] = tpe match {
        case TypeRef(x, _, a :: b :: _) => a :: hunfold(b)
        case x => Nil
      }

      def hselect(tree: Tree, i: Int): Tree = i match {
        case 0 => Select(tree, TermName("head"))
        case i => hselect(Select(tree, TermName("tail")), i - 1)
      }

      val xs = hunfold(c.weakTypeOf[OL])
      val Apply(Apply(TypeApply(_, _), List(opticsVal)), _) = c.prefix.tree

      _compose[O](c)(xs)(hselect(opticsVal, _))
    }


    def _compose[O: c.WeakTypeTag](c: Context)(xs: List[c.Type])(get: Int => c.Tree): c.Expr[O] = {
      import c.universe._
      import Flag._

      // TODO Move to macro utils
      def unapply4(tpe: Type): Option[(Type, Type, Type, Type)] = tpe match {
        case TypeRef(x, _, a :: b :: c :: d :: _) => Some((a, b, c, d))
        case _                          => None
      }

      val o = c.weakTypeOf[O]
      val Some((s, t, a, b)) = unapply4(o)
      val is = (0 to (xs.size -1))

      val getter = is.reverse.foldLeft(Ident(TermName("s")): Tree) { (tree, i) =>
        Apply(Select(get(i), TermName("get")), List(tree))
      }

      val setter = {
        def init(s: TermName) = Apply(Select(get(0), TermName("set")), List(Ident(s), Ident(TermName("b"))))
        is.tail.foldLeft(init(_: TermName)) { (f, i) =>
          def tree1(s: TermName) = {
            val a = TermName(c.freshName("a"))
            val fun = Function(List(ValDef(Modifiers(PARAM), a, TypeTree(), EmptyTree)), f(a))
            Apply(Apply(Select(get(i), TermName("modify")), List(fun)), List(Ident(s)))
          }
          tree1(_: TermName)
        }.apply(TermName("s"))
      }

      val className = TypeName(c.freshName("$lens"))

      val classDef = {
        val constructor =
          DefDef(
            Modifiers(), termNames.CONSTRUCTOR, Nil,
            List(Nil),
            TypeTree(),
            Block(
              List(Apply(Select(Super(This(typeNames.EMPTY), typeNames.EMPTY), termNames.CONSTRUCTOR), List())),
              Literal(Constant(()))
            )
          )

        val get = DefDef(
          Modifiers(), TermName("get"), Nil,
          List(List(ValDef(Modifiers(PARAM), TermName("s"), TypeTree(s), EmptyTree))),
          TypeTree(a),
          getter
        )

        val set = DefDef(
          Modifiers(), TermName("set"), List(),
          List(
            List(
              ValDef(Modifiers(PARAM), TermName("s"), TypeTree(s), EmptyTree),
              ValDef(Modifiers(PARAM), TermName("b"), TypeTree(b), EmptyTree)
            )
          ),
          TypeTree(t),
          setter
        )

        ClassDef(
          Modifiers(FINAL), className, Nil,
          Template(List(TypeTree(o)), noSelfType, List(constructor, get, set))
        )
      }

      val tree = Block(List(classDef), Apply(Select(New(Ident(className)), termNames.CONSTRUCTOR), Nil))

      c.Expr[O](tree)
    }
  }
}
