package scalaz

abstract class Show[A] {
  def show(a: A): String
}

object Show {
  def mkShow[A](f: A => String): Show[A] = new Show[A] { override def show(a: A) = f(a) }
  def mkShowFromToString[A]: Show[A] = mkShow(_.toString)

  def show[A](a: A)(implicit A: Show[A]): String = A.show(a)

  implicit val showBoolean: Show[Boolean] = mkShowFromToString
  implicit val showInt: Show[Int] = mkShowFromToString
  implicit val showThrowable: Show[Throwable] = mkShowFromToString
  implicit val showString: Show[String] = mkShowFromToString

  import scala.language.implicitConversions

  implicit def showList[A: Show](xs: List[A]): Show[List[A]] = mkShow(_.map(show(_)).mkString("(", ", ", ")"))
  implicit def showOption[A: Show](oa: Option[A]): Show[Option[A]] = mkShow(_.map(show(_)).mkString("(", ", ", ")"))
  implicit def showTuple2[A](implicit showA: Show[A]): Show[(A, A)] =
    mkShow { case (a1, a2) => (showA.show(a1), showA.show(a2)).toString }
}
