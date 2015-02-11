package acme
package deriving

import scalaz._
import system.IO._

object Demo extends App {
  sealed trait Foo
  case object Bar extends Foo
  case object Baz extends Foo

  implicit val enum: Enum[Foo] = Deriving.mkEnum[Foo]

  // scala> enum.all
  // res0: List[Foo] = List(Bar, Baz)

  case class Person(name: String, age: Int)
  object Person { implicit val show: Show[Person] = Deriving.mkShow[Person] }

  case class Cons[A](head: A, tail: Option[Cons[A]])
  object Cons {
    import scala.reflect.runtime.universe._
    //implicit def show[A: Show: TypeTag]: Show[Cons[A]] = Deriving.mkShow[Cons[A]]

    implicit def show[A: Show: TypeTag]: Show[Cons[A]] = {
      val derivable = Derivable.show
      derivable.instance[Cons[A]] { b =>
        derivable.reduce[Cons[A]](List(
          derivable.inject[A].apply(b.head)
        ))
      }
    }
  }

  def mainIO: IO[Unit] =
    print(Person("Alice", 17)) *>
    print(Cons("auto", Some(Cons("deriving", None))))

  // scala> main(Array())
  // Person(Alice, 17)

}


