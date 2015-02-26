package scalaz

object YOLO {
  def ???[A]: A = sys.error("Not implemented yet.")
  def println(string: String) = scala.Predef.println(string)
  def undefined[A]: A = ???[A]
}
