package scalaz
package data

abstract class Lens[S, T, A, B] {
  def get(s: S): A
  def set(s: S, b: B): T

  final def modify(f: A => B)(s: S): T = set(s, f(get(s)))
}
