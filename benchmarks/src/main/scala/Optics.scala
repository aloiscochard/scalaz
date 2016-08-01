package scalaz
package benchmarks

/*
 * [info] # Run complete. Total time: 00:04:02
[info]
[info] Benchmark                  Mode  Cnt         Score         Error  Units
[info] OpticsScalaz.get          thrpt   20  77312790.898 ± 1725642.380  ops/s
[info] OpticsScalaz.modify       thrpt   20   9289954.263 ±  269677.445  ops/s
[info] OpticsScalaz.set          thrpt   20   9602764.823 ±  337762.087  ops/s
*/

object Optics extends LensModel {
  val init = oi(42)

  object Scalaz {
    import data._
    import data.Lens._

    /*
    val _01 = new Lens[O0, O0, O1, O1] { def get(o: O0): O1 = o.o; def set(o: O0, p: O1): O0 = o.copy(o = p) }
    val _12 = new Lens[O1, O1, O2, O2] { def get(o: O1): O2 = o.o; def set(o: O1, p: O2): O1 = o.copy(o = p) }
    val _23 = new Lens[O2, O2, O3, O3] { def get(o: O2): O3 = o.o; def set(o: O2, p: O3): O2 = o.copy(o = p) }
    val _34 = new Lens[O3, O3, O4, O4] { def get(o: O3): O4 = o.o; def set(o: O3, p: O4): O3 = o.copy(o = p) }
    val _45 = new Lens[O4, O4, O5, O5] { def get(o: O4): O5 = o.o; def set(o: O4, p: O5): O4 = o.copy(o = p) }
    val _56 = new Lens[O5, O5, O6, O6] { def get(o: O5): O6 = o.o; def set(o: O5, p: O6): O5 = o.copy(o = p) }
    val _67 = new Lens[O6, O6, O7, O7] { def get(o: O6): O7 = o.o; def set(o: O6, p: O7): O6 = o.copy(o = p) }
    val _78 = new Lens[O7, O7, O8, O8] { def get(o: O7): O8 = o.o; def set(o: O7, p: O8): O7 = o.copy(o = p) }
    val _89 = new Lens[O8, O8, O9, O9] { def get(o: O8): O9 = o.o; def set(o: O8, p: O9): O8 = o.copy(o = p) }
    val _i = new Lens[O9, O9, Int, Int] { def get(o: O9): Int = o.i; def set(o: O9, i: Int): O9 = o.copy(i = i) }
    */

    val _01 = slens[O0, O1](_.o)(o => p => o.copy(o = p))
    val _12 = slens[O1, O2](_.o)(o => p => o.copy(o = p))
    val _23 = slens[O2, O3](_.o)(o => p => o.copy(o = p))
    val _34 = slens[O3, O4](_.o)(o => p => o.copy(o = p))
    val _45 = slens[O4, O5](_.o)(o => p => o.copy(o = p))
    val _56 = slens[O5, O6](_.o)(o => p => o.copy(o = p))
    val _67 = slens[O6, O7](_.o)(o => p => o.copy(o = p))
    val _78 = slens[O7, O8](_.o)(o => p => o.copy(o = p))
    val _89 = slens[O8, O9](_.o)(o => p => o.copy(o = p))
    //val _i = new Lens[O9, O9, Int, Int] { def get(o: O9): Int = o.i; def set(o: O9, i: Int): O9 = o.copy(i = i) }
    val _i  = slens[O9, Integer](_.i)(o => i => o.copy(i = i))

    val _baseline = slens[O0, Int](_.o.o.o.o.o.o.o.o.o.i)(o => i =>
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

    val _telephoto = Telephoto.compose(_01, _12, _23, _34, _45, _56, _67, _78, _89, _i)

    val _telephotoHList = {
      import data.HList._

      Telephoto(
        HCons(_i, HCons(_89, HCons(_78, HCons(_67, HCons(_56,
          HCons(_45, HCons(_34, HCons(_23, HCons(_12, HCons(_01, HNil))))))))))
      ).optic
    }
  }

  object Monocle {
    import monocle._

    val _01 = Lens[O0, O1](_.o)(p => o => o.copy(o = p))
    val _12 = Lens[O1, O2](_.o)(p => o => o.copy(o = p))
    val _23 = Lens[O2, O3](_.o)(p => o => o.copy(o = p))
    val _34 = Lens[O3, O4](_.o)(p => o => o.copy(o = p))
    val _45 = Lens[O4, O5](_.o)(p => o => o.copy(o = p))
    val _56 = Lens[O5, O6](_.o)(p => o => o.copy(o = p))
    val _67 = Lens[O6, O7](_.o)(p => o => o.copy(o = p))
    val _78 = Lens[O7, O8](_.o)(p => o => o.copy(o = p))
    val _89 = Lens[O8, O9](_.o)(p => o => o.copy(o = p))
    val _i  = Lens[O9, Int](_.i)(i => o => o.copy(i = i))

    val _compose = _01 composeLens _12 composeLens _23 composeLens _34 composeLens _45 composeLens _56 composeLens _67 composeLens _78 composeLens _89 composeLens _i
  }
}
