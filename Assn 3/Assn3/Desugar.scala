package Assignment3.Desugar
import scala.collection.immutable.ListMap
import Assignment3.Source._
import Assignment3.Core._

// Desugaring
object Desugar {
  import Source._

  def toCoreOp(op: BinaryOp): Core.BinaryOp = op match {
    case AddOp => Core.AddOp
    case MulOp => Core.MulOp
    case SubOp => Core.SubOp
    case DivOp => Core.DivOp
    case AndOp => Core.AndOp
    case OrOp => Core.OrOp
    case EqOp => Core.EqOp
  }

  def desugar(e: Expr): Core.Expr = e match {
    case Var(v) => Core.Var(v)

    case Class(selfBnd, selfTy, extendsData, methods, overridden) => {
        // Here, we suggest a skeleton implementation. Feel free to rewrite
        // this with your own, if you like!

        val (superclass, superty) = extendsData match {
          case Some((expr, ty)) => (expr, ty)
          case None => (RootClass, TyClass(TyObject("X", ListMap())))
        }

        val superFields = superty match {
          case TyClass(TyObject(_, fields)) => fields
          case _ =>
            sys.error("Error: supertype isn't an object. This shouldn't happen with well-typed programs.")
        }

        def getInheritedLabels(): Set[Label] = {
          sys.error("TODO: Fill me in!")
        }

        def generateNewMethod(): Core.Method = {
          sys.error("TODO: Fill me in!")
        }

        // Generate fields for inherited (non-defined) methods
        def generateInheritedMethods(inheritedLabels: Set[Label]): ListMap[Core.Label, Core.Method] = {
          sys.error("TODO: Fill me in!")
        }

        def generateDeclaredMethods(): ListMap[Core.Label, Core.Method] = {
          sys.error("TODO: Fill me in!")
        }

        def generateOverriddenMethods(): ListMap[Core.Label, Core.Method] = {
          sys.error("TODO: Fill me in!")
        }


        // Calculate the set of inherited labels (i.e. those which appear in the
        // superclass type, but are not defined or overridden)
        val inheritedLabels = getInheritedLabels()

        // Generate the "new" method
        val newMethod = generateNewMethod()

        // Generate the inherited methods
        val inheritedMethods = generateInheritedMethods(inheritedLabels)

        // Generate the declared methods
        val declaredMethods = generateDeclaredMethods()

        // Generate the overridden methods
        val overriddenMethods = generateOverriddenMethods()

        // Fields = new method + inherited methods + overridden methods
        val fields = ListMap("new" -> newMethod) ++ inheritedMethods ++ declaredMethods ++ overriddenMethods
        Core.Object(fields)
    }

    case IfThenElse(e1, e2, e3) =>
      Core.IfThenElse(desugar(e1), desugar(e2), desugar(e3))

    case Num(n) => Core.NumV(n)

    case _ => sys.error("TODO: Fill me in!")
  }
}

// vim: set ts=2 sw=2 et sts=2:
