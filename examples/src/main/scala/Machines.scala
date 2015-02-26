package acme

import scalaz._
import monad.Free._

import machines._
import Machine._
import Step._

import YOLO.undefined

object Machines {

  val machine: Process[Int, Int] = MachineT[Trampoline, Is[Int, ?], Int](run = Trampoline.done(
    new Await[Is[Int, ?], Int, MachineT[Trampoline, Is[Int, ?], Int]] {
      def g[T]: T => MachineT[Trampoline, Is[Int, ?], Int] = undefined
      def kg[T]: Is[Int, T] = Is[Int, T]
      def r: MachineT[Trampoline, Is[Int, ?], Int] = undefined

      //def map[B](f: MachineT[Trampoline, Is[Int, ?], Int] => B): Await[Is[Int, ?], Int, B] = undefined

      /*
def map[B](f: scalaz.machines.MachineT[scalaz.monad.Free.Trampoline,[X_kp1]scalaz.machines.Machine.Is[Int,X_kp1],Int] => B): scalaz.machines.Step[[X_kp1]scalaz.machines.Machine.Is[Int,X_kp1],Int,B] = ???

*/
      /*
      def apply[T]: 2
      def g[T]: T => MachineT[Trampoline, Is[Int, Int], Int]
      def kg[T]: K[T]
      def r: MachineT[Trampoline, Is[Int, Int], Int]
      */
    }

  ))

  //case class MachineT[M[_], K[_], O](run: M[Step[K, O, MachineT[M, K, O]]])
}
