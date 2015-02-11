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

  case class Cons(name: String, tail: Option[Cons])
  object Cons {
    implicit def show: Show[Cons] = Deriving.mkShow[Cons]
    /*
    implicit def show: Show[Cons] = Derivable.show.instance[Cons] { b =>
      Derivable.show.reduce[Cons](List(
        Derivable.show.inject[String].apply(b.name),
        Derivable.show.inject[Option[Cons]].apply(b.tail)
      ))
    }
    */
  }

    /*
    implicit val show: Show[Person] = Derivable.show.instance[Person] { b =>
      Derivable.show.reduce[Person](List(
        Derivable.show.inject[String].apply(b.name),
        Derivable.show.inject[Int].apply(b.age)
      ))
    }
    */
  def mainIO: IO[Unit] =
    print(Person("Alice", 17)) *>
    print(Cons("auto", None))
    //print(Cons("auto", Some(Cons("deriving", None))))

  // scala> main(Array())
  // Person(Alice, 17)

}


