package scalaz

case class Identity[A](run: A) extends AnyVal
