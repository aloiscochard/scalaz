package scalaz
package benchmarks

import data.Lens._

trait LensModel {
  case class O0(o: O1)
  case class O1(o: O2)
  case class O2(o: O3)
  case class O3(o: O4)
  case class O4(o: O5)
  case class O5(o: O6)
  case class O6(o: O7)
  case class O7(o: O8)
  case class O8(o: O9)
  case class O9(i: Int)

  val _01 = slens[O0, O1](_.o)(o => p => o.copy(o = p))
  val _12 = slens[O1, O2](_.o)(o => p => o.copy(o = p))
  val _23 = slens[O2, O3](_.o)(o => p => o.copy(o = p))
  val _34 = slens[O3, O4](_.o)(o => p => o.copy(o = p))
  val _45 = slens[O4, O5](_.o)(o => p => o.copy(o = p))
  val _56 = slens[O5, O6](_.o)(o => p => o.copy(o = p))
  val _67 = slens[O6, O7](_.o)(o => p => o.copy(o = p))
  val _78 = slens[O7, O8](_.o)(o => p => o.copy(o = p))
  val _89 = slens[O8, O9](_.o)(o => p => o.copy(o = p))

  val _i  = slens[O9, Int](_.i)(o => i => o.copy(i = i))

  val _0i = _01 ∘ _12 ∘ _23 ∘ _34 ∘ _45 ∘ _56 ∘ _67 ∘ _78 ∘ _89 ∘ _i

  val _0iTarget = slens[O0, Int](_.o.o.o.o.o.o.o.o.o.i)(o => i =>
      o.copy(o =
        o.o.copy(o =
          o.o.o.copy(o =
            o.o.o.o.copy(o =
              o.o.o.o.o.copy(o =
                o.o.o.o.o.o.copy(o =
                  o.o.o.o.o.o.o.copy(o =
                    o.o.o.o.o.o.o.o.copy(o =
                      o.o.o.o.o.o.o.o.o.copy(o =
                        o.o.o.o.o.o.o.o.o.o.copy(i = i)))))))))))

  import meta.HList._

  val _0iScope = data.Optic.Scope.run(
    HCons(_01, HCons(_12, HCons(_23, HCons(_34, HCons(_45,
      HCons(_56, HCons(_67, HCons(_78, HCons(_89, HCons(_i, HNil))))))))))
  )
}
