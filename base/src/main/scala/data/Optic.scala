package scalaz
package data

object Optic extends OpticTypes
              with PrismFunctions {

  case class Scope[Optics <: meta.HList](optics: Optics)

  object Scope {
    import scala.language.experimental.macros
    def run[Optics <: meta.HList, A](optics: Optics): A = macro meta.Optics.run[Optics, A]
  }
}
