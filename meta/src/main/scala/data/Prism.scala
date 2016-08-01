package scalaz
package data

case class Prism[S, T, A, B](reverseGet: B => T, getOrModify: S => Either[T, A])
