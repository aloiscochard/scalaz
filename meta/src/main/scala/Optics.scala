package scalaz
package meta

import scala.reflect.macros.whitebox.Context

object Optics {
  def run[Optics <: HList, A](c: Context)(optics: c.Expr[Optics]): c.Expr[A] = {
    ???
  }
}
