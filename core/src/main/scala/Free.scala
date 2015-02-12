package scalaz

import Either._
// TODO Create a common trait to share interpreter code? (with IO)

trait Free[F[_], A]
//case class Free[F[_], A](thunk: Free.Spec.Thunk)

object Free {
  case class Pure[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](ffa: F[Free[F, A]]) extends Free[F, A]

  /*
  def pureF[F[_], A](a: => A): Free[F, A] = Free[F, A](Spec.Thunk.pure[A](_ => a))
  def suspendF[F[_], A](ffa: F[Free[F, A]]): Free[F, A] = Free[F, A](Spec.Thunk.suspend[F, A](ffa))
  */

  /*
  object Spec {
    type Val = Any with ({ type Tag = Any })

    object Val {
      //val unit: Val = cast(Unit)
      def cast(x: Any): Val = x.asInstanceOf[Val]
      def castF[F[_], A](ffa: F[Free[F, A]]): Val = ffa.asInstanceOf[Val]
      def reify[A](x: Val): A = x.asInstanceOf[A]
      def reifyF[F[_], A](x: Val): F[Free[F, A]] = x.asInstanceOf[F[Free[F, A]]]
    }

    type Thunk = List[Exp]

    object Thunk {
      import Exp._; import Val._;

      def pure[A](f: Lazy[A]): Thunk = Pure((_: Unit) => cast(f(()))) :: Nil
      def suspend[F[_], A](ffa: F[Free[F, A]]): Thunk = Suspend(castF[F, A](ffa)) :: Nil
    }

    sealed trait Exp
    object Exp {
      case class Pure(a: Lazy[Val]) extends Exp
      case class Suspend(ffa: Val) extends Exp
    }
  }
  */

  object Iter {
    def iter[F[_], A](phi: F[A] => A)(ffa: Free[F, A])(implicit F: Functor[F]): A = ffa match {
      case Pure(a)    => a
      case Suspend(m) => phi(m.map(iter(phi)(_)))
    }

    def iterA[G[_]: Applicative, F[_]: Functor, A](phi: F[G[A]] => G[A])(ffa: Free[F, A]): G[A] = ffa match {
      case Pure(x)    => x.pure[G]
      case Suspend(m) => phi(m.map(iterA(phi)(_)))
    }
  }

  /*

  type Trampoline[A] = Free[Lazy, A]

  object Trampoline {
    def done[A](a: A): Trampoline[A] = Pure[Lazy, A](a)
    def suspend[A](a: => Trampoline[A]): Trampoline[A] = Suspend[Lazy, A](() => a)

    def run[F[_]: Functor, A](phi: F[A] => A)(ffa: Free[F, A]): A = {
      var thunk: Thunk = ffa.thunk.reverse
      var value: A = null.asInstanceOf[A]

      def go(fffa: F[Free[F, A]]): F[Free[F, A]] \/ A = fffa match {
        case Pure(a) => reify[A](a(())).right[F[Free[F, A]]]
        case Suspend(m) => reifyF[F, A](m).left[A]
      }

      while(value != null) {
        go(thunk.head) match {
          case Right(a) =>
            value = a
          case Left(ffa) =>
            thunk = ffa.thunk
        }
      }

      value
    }
  }


  implicit def functor[F[_], A](implicit F: Functor[F]): Functor[Free[F, A]] = new Functor[Free[F, A]] {
  }


instance Functor f => Functor (Free f) where
  fmap f = go where
    go (Pure a)  = Pure (f a)
    go (Free fa) = Free (go <$> fa)
  {-# INLINE fmap #-}

instance Functor f => Apply (Free f) where
  Pure a  <.> Pure b = Pure (a b)
  Pure a  <.> Free fb = Free $ fmap a <$> fb
  Free fa <.> b = Free $ (<.> b) <$> fa

instance Functor f => Applicative (Free f) where
  pure = Pure
  {-# INLINE pure #-}
  Pure a <*> Pure b = Pure $ a b
  Pure a <*> Free mb = Free $ fmap a <$> mb
  Free ma <*> b = Free $ (<*> b) <$> ma

instance Functor f => Bind (Free f) where
  Pure a >>- f = f a
  Free m >>- f = Free ((>>- f) <$> m)

instance Functor f => Monad (Free f) where
  return = Pure
  {-# INLINE return #-}
  Pure a >>= f = f a
  Free m >>= f = Free ((>>= f) <$> m)
  */
}
