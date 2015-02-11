package scalaz

import scala.language.experimental.macros
import scala.reflect.runtime.universe._
import scala.reflect.macros.blackbox.Context

trait Derivable[TC[_], S[_, _], A] {
  def instance[B: TypeTag](f: S[B, A]): TC[B]
  def reduce[B](xs: List[A]): A
  def inject[B: TC]: S[B, A]
}

object Derivable {
  implicit val show = new Derivable[Show, ? => ?, String] {
    override def instance[B: TypeTag](f: B => String): Show[B] = new Show[B] {
      def show(b: B): String = typeTag[B].tpe.typeSymbol.name.toString + f(b)
    }
    override def reduce[B](xs: List[String]): String = xs.mkString("(", ", ", ")")
    override def inject[B](implicit tc: Show[B]): B => String = tc.show(_)
  }

  implicit val equal = new Derivable[Equal, λ[(A, B) => (A, A) => B], Boolean] {
    override def instance[B: TypeTag](f: (B, B) => Boolean): Equal[B] = new Equal[B] {
      def equal(b1: B, b2: B): Boolean = f(b1, b2)
    }
    override def reduce[B](xs: List[Boolean]): Boolean = xs.reduce(_ && _)
    override def inject[B](implicit tc: Equal[B]): (B, B) => Boolean = tc.equal(_, _)
  }
}

