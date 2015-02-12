package scalaz

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

class z extends scala.annotation.StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro z.apply
}

object z {
  def apply(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import c.universe.Flag._

    val transform: DefDef => DefDef = _ match {
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        val vals = vparamss.flatMap(id).filter(_.mods.hasFlag(IMPLICIT))

        val implicits: List[Tree] = vals.flatMap { v =>
          val tree = v.tpt
          val AppliedTypeTree(ident, _) = tree
          val tpe = c.typecheck(ident, c.TYPEmode).tpe
          tpe.decls.collect { case m: MethodSymbol if m.isImplicit && m.paramLists.isEmpty =>
            val n = s"${v.name}_${m.name}"
            val i = Select(Ident(v.name), m.name)
            DefDef(Modifiers(IMPLICIT), TermName(n), List(), List(), TypeTree(), i)
          }
        }

        DefDef(mods, name, tparams, vparamss, tpt, q"{..${implicits :+ rhs}}")
    }

    val transformTmp: Template => Template = _ match {
      case Template(parents, self, body) => Template(parents, self, body.collect {
        case d: DefDef => transform(d)
        case t => t
      })
    }

    val trees = annottees.map( _.tree match {
      case ClassDef(mods, name, tparams, tmp) =>
        ClassDef(mods, name, tparams, transformTmp(tmp))
      case ModuleDef(mods, name, tmp) =>
        ModuleDef(mods, name, transformTmp(tmp))
      case d: DefDef => transform(d)
      case tree =>
        c.warning(c.enclosingPosition, "The annotation have no effect here.")
        tree
    })

    c.Expr[Any](q"{ ${trees:_*} }")
  }
}
