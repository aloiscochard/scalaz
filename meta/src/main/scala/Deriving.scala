package scalaz

import scala.language.experimental.macros
import scala.reflect.runtime.universe._
import scala.reflect.macros.blackbox.Context

object Deriving {

//trait Derivable[TC[_], S[_, _], A] {


  import Derivable._

  def mkShow[A]: Show[A] = macro derive[Show, ? => ?, String, A]

  case class Person(name: String, age: Int)


  def derive[TC[_], S[_, _], A, T](c: Context)(implicit
    TC: c.WeakTypeTag[TC[Nothing]],
    S: c.WeakTypeTag[S[Nothing, Nothing]],
    A: c.WeakTypeTag[A],
    T: c.WeakTypeTag[T]
  ): c.Expr[TC[T]] = {
    import c.universe._
    import c.universe.Flag._
    val tpe = weakTypeOf[T]
    val symbol = tpe.typeSymbol
    val internal = symbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol]

    // TODO Add logic for sumtypes
    if (internal.isSealed) c.abort(c.enclosingPosition, "Sum types are not yet supported.")

    // TODO Why this does not work? /ask @xeno_by
    //def typeCons[T: WeakTypeTag] = weakTypeOf[T].typeConstrutor

    val derivable = c.inferImplicitValue(appliedType(
      typeOf[Derivable[Nothing, Nothing, Nothing]].typeConstructor,
      List(weakTypeOf[TC[Nothing]].typeConstructor, weakTypeOf[S[Nothing, Nothing]].typeConstructor, weakTypeOf[A].typeConstructor)
    ))

    val fields = tpe.declarations.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor ⇒ m
    }.get.paramss.head

    import scala.Predef.println

    val injects = fields.map { sym =>
      val injectType = sym.typeSignature
      // TODO Why does it not fail if not found?
      val injectInstance = c.inferImplicitValue(appliedType(weakTypeOf[TC[Nothing]], injectType))

      if (injectInstance.isEmpty) {
        c.abort(c.enclosingPosition,
          s"Could not find instance for `${injectType}`.\n\n" +
          "You are most likely trying to derive a complex recursive type,\n" +
          "they are not supported so I'm afraid you'll have to tie the knot by hand.\n "
        )
      }

      Apply(
        Select(
          Apply(
            TypeApply(
              Select(Ident(TermName("derivable")), TermName("inject")),
              List(TypeTree(injectType))
            ),
            List(injectInstance)
          ),
          TermName("apply")
        ),
        List(Select(Ident(TermName("b")), sym.name))
      )
    }

    /*

    println(showRaw(reify {

      import scala.concurrent.Future
      val derivable = Derivable.show

      derivable.instance[Person] { b =>
        derivable.reduce[Person](List(
          derivable.inject[String].apply(b.name),
          derivable.inject[Int].apply(b.age)
        ))
      }
    }.tree))
    */

    // tpe = List(Select(Ident(scalaz.Show), TermName("showString")))
    // fieldName = TermName("name")
    def inject(instance: Tree, tpe: Tree, field: Symbol): Tree =
      Apply(
        Select(
          Apply(
            TypeApply(
              Select(Ident(TermName("derivable")), TermName("inject")),
              List(tpe)
            ),
            List(instance)
          ),
          TermName("apply")
        ),
        List(Select(Ident(TermName("b")), field))
      )
    /*
      Apply(
        Select(
          Apply(
            TypeApply(
              Select(Ident(TermName("derivable")), TermName("inject")),
              List(Ident(java.lang.String))
            ),
            List(Select(Ident(scalaz.Show), TermName("showString")))
          ),
          TermName("apply")
        ),
        List(Select(Ident(TermName("b")), TermName("name")))
      ),
      */

    c.Expr[TC[T]](Block(
      List(ValDef(Modifiers(), TermName("derivable"), TypeTree(), derivable)),
      Apply(
        Apply(
          TypeApply(
            Select(Ident(TermName("derivable")), TermName("instance")),
            List(TypeTree(weakTypeOf[T]))
          ),
          List(
            Function(
              List(ValDef(Modifiers(PARAM), TermName("b"), TypeTree(), EmptyTree)),
              Apply(
                TypeApply(
                  Select(Ident(TermName("derivable")), TermName("reduce")),
                  List(TypeTree(weakTypeOf[T]))
                ),
                List(
                  Apply(
                    Select(Ident("List"), TermName("apply")),
                    injects
                  )
                )
              )
            )
          )
        ),
        List(Select(Select(Ident(TermName("scala")), TermName("Predef")), TermName("implicitly")))
      )
    ))
  }

  /*
Block(
  List(ValDef(Modifiers(), TermName("derivable"), TypeTree(), Select(Ident(scalaz.Derivable), TermName("show")))),
  Apply(
    Apply(
      TypeApply(
        Select(Ident(TermName("derivable")), TermName("instance")),
        List(Select(This(TypeName("Deriving")), TypeName("Person")))
      ),
      List(
        Function(
          List(ValDef(Modifiers(PARAM), TermName("b"), TypeTree(), EmptyTree)),
          Apply(
            TypeApply(
              Select(Ident(TermName("derivable")), TermName("reduce")),
              List(Select(This(TypeName("Deriving")), TypeName("Person")))
            ),
            List(
              Apply(
                Select(Ident(scala.collection.immutable.List), TermName("apply")),
                List(
                  Apply(
                    Select(
                      Apply(
                        TypeApply(
                          Select(Ident(TermName("derivable")), TermName("inject")),
                          List(Ident(java.lang.String))
                        ),
                        List(Select(Ident(scalaz.Show), TermName("showString")))
                      ),
                      TermName("apply")
                    ),
                    List(Select(Ident(TermName("b")), TermName("name")))
                  ),
                  Apply(
                    Select(
                      Apply(
                        TypeApply(Select(Ident(TermName("derivable")), TermName("inject")), List(Ident(scala.Int))),
                        List(Select(Ident(scalaz.Show), TermName("showInt")))
                      ),
                      TermName("apply")
                    ),
                    List(Select(Ident(TermName("b")), TermName("age")))
                  )
                )
              )
            )
          )
        )
      )
    ),
    List(Select(This(TypeName("Predef")), TermName("implicitly")))
  )
)
*/

  def mkEnum[A]: Enum[A] = macro deriveEnum[A]

  def deriveEnum[A: c.WeakTypeTag](c: Context): c.Expr[Enum[A]] = {
    import c.universe._
    val symbol = weakTypeOf[A].typeSymbol
    val internal = symbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol]
    if (!internal.isSealed) c.abort(c.enclosingPosition, s"The type `${symbol.name}` is not sealed, unable to derive `Enum`.")

    val descendants = internal.sealedDescendants.map(_.asInstanceOf[Symbol])

    // TODO Fail if a descendants is not an `object`.
    val objs = (descendants - symbol).map(
      s => s.owner.typeSignature.member(s.name.toTermName)
    )

    // TODO Better error message, explaining the limitation of the implementation.
    if (objs.isEmpty)
      c.abort(c.enclosingPosition, s"No objects of type `${symbol.name}` found.")

    c.Expr[Enum[A]](
      Apply(
        Select(Select(Ident(TermName("scalaz")), TermName("Enum")), TermName("fromList")),
        List(
          Apply(
            Select(Ident(TermName("List")), TermName("apply")),
            objs.map(Ident(_)).to[List]
          )
        )
      )
    )
  }
}
