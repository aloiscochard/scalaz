package scalaz
package data

//case class Telephoto[Optics <: HList](optics: Optics)

trait Telephoto[Optics, A] {
  def apply(optics: Optics): A
}

object Telephoto {
  import scala.language.experimental.macros
  import scala.reflect.macros.whitebox.Context


  def runOptics[Optics <: HList, A](optics: Optics)(implicit Optics: Telephoto[Optics, A]): A = Optics(optics)
}
