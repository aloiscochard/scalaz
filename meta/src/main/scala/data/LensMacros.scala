package scalaz
package data

import scala.reflect.macros.whitebox.Context

object LensMacros {
  def slens[S: c.WeakTypeTag, A: c.WeakTypeTag](c: Context)
      (sa: c.Expr[S => A])(sas: c.Expr[S => A => S]): c.Expr[Lens[S, S, A, A]] =
    lens[S, S, A, A](c)(sa)(sas)

  def lens[S: c.WeakTypeTag, T: c.WeakTypeTag, A: c.WeakTypeTag, B: c.WeakTypeTag](c: Context)
      (sa: c.Expr[S => A])(sbt: c.Expr[S => B => T]): c.Expr[Lens[S, T, A, B]] = {
    import c.universe._
    import Flag._

    val (s, t, a, b) = (c.weakTypeOf[S], c.weakTypeOf[T], c.weakTypeOf[A], c.weakTypeOf[B])

    c.Expr(q"""
      new Lens[$s, $t, $a, $b] {
          def get(s: $s): $a = $sa(s)
          def set(s: $s, b: $b): $t = $sbt(s)(b)
        }
    """)
  }
}
