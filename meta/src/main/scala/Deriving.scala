package scalaz

import scala.language.experimental.macros
import scala.reflect.runtime.universe._
import scala.reflect.macros.blackbox.Context

object Deriving {
  import Derivable._

  def mkShow[A]: Show[A] = macro derive[Show, ? => ?, String, A]

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

    val fields = tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor ⇒ m
    }.get.paramLists.head

    import scala.Predef.println

    //val descendants = internal.sealedDescendants.map(_.asInstanceOf[Symbol])

    val injects = fields.map { sym =>
      val injectType = sym.typeSignature
      val injectInstanceType = appliedType(weakTypeOf[TC[Nothing]].typeConstructor, injectType)
      val injectInstance = c.inferImplicitValue(injectInstanceType)

      if (injectInstance.isEmpty) {
        val tc = appliedType(weakTypeOf[TC[Nothing]]).typeConstructor
        c.abort(c.enclosingPosition, s"Could not resolve `${injectInstanceType}` instance.")
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

    c.Expr[TC[T]](Block(
      List(q"val derivable = $derivable"),
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
                    Select(Ident(TermName("List")), TermName("apply")),
                    injects
                  )
                )
              )
            )
          )
        ),
        List(q"implicitly")
      )
    ))
  }

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
