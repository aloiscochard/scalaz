package scalaz
package machines

import monad.Free.Trampoline

case class MachineT[M[_], K[_], O](run: M[Step[K, O, MachineT[M, K, O]]])

object Machine {
  import Step._
  //type Machine[K[_], O] = MachineT[M forSome { type M[_] }, K, O]
  type Machine[K[_], O] = MachineT[Trampoline, K, O]

  final class Is[A, B]
  object Is {
    def apply[A, B] = new Is[A, B]
  }

  type Process[A, B] = Machine[Is[A, ?], B]
  type ProcessT[M[_], A, B] = MachineT[M, Is[A, ?], B]

  def runMachine[K[_], B](mt: MachineT[Trampoline, K, B])(implicit M: Monad[Trampoline]): Seq[B] =
    runMachineT[Trampoline, K, B](mt).run

  def runMachineT[M[_], K[_], B](mt: MachineT[M, K, B])(implicit M: Monad[M]): M[Seq[B]] = {
    import M._
    mt.run.flatMap(_ match {
      case Stop()                                 => Seq.empty.pure[M]
      case Yield(o, k)                            => runMachineT(k).map(o+:_)
      case await: Await[K, B, MachineT[M, K, B]]  => runMachineT(await.r)
    })
  }
}
