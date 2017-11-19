package Assignment3.Desugar
import scala.collection.immutable.ListMap
import Assignment3.Source._
import Assignment3.Core._
import Assignment3.Utility.Utility._

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
    
    case Bool(b) => Core.BoolV(b)

    case Str(s) => Core.StrV(s)

    case BinOp(op,e1,e2) => op match {
      case AddOp => Core.BinOp(Core.AddOp,desugar(e1),desugar(e2))
      case SubOp => Core.BinOp(Core.SubOp,desugar(e1),desugar(e2))
      case MulOp => Core.BinOp(Core.MulOp,desugar(e1),desugar(e2))
      case DivOp => Core.BinOp(Core.DivOp,desugar(e1),desugar(e2))
      case AndOp => Core.BinOp(Core.AndOp,desugar(e1),desugar(e2))
      case OrOp => Core.BinOp(Core.OrOp,desugar(e1),desugar(e2))
      case EqOp => Core.BinOp(Core.EqOp,desugar(e1),desugar(e2))
    }
  
    case NotOp(e) => Core.NotOp(desugar(e))

    case LetIn(x,e1,e2) => Core.LetIn(x,desugar(e1),desugar(e2))

    case Object(selfBinder,selfTy,fields) => {
      var newMethods = scala.collection.mutable.ListMap[Core.Label,Core.Method]()      
      for ((label,expr) <- fields)
        newMethods += (label -> Core.Method(selfBinder,desugar(expr)))         
      val new_methods = ListMap(newMethods.toList : _*)      
      Core.Object(new_methods)
    }  

    case RootClass => {
      val x = Gensym.gensym("X")
      Core.Object( ListMap[Core.Label,Core.Method]("new"->Core.Method(x,Core.Object(new ListMap[Core.Label, Core.Method]()))))
    }

    case Func(params,body) => {
      var l = Core.Lambda(params.reverse(0)._1,desugar(body))
      for ((variable,_) <- params.reverse.drop(1)) {
        l = Core.Lambda(variable,l)
      }
      l
    }

    case Apply(e,args) => {
      var ap = Core.Apply(desugar(e),desugar(args(0)))
      val args2 = args.drop(1)
      for (i <- args2) {
        ap = Core.Apply(ap,desugar(i))    // Desugar ap as well??
      }
      ap
    }

    case New(e) => Core.Invoke(desugar(e),"new")

    case SelectField(obj,label) => Core.Invoke(desugar(obj),label)

    case FieldUpdate(obj,label,newBody) => {
      val x = Gensym.gensym("x")
      Core.Update(desugar(obj),label,Core.Method(x,desugar(newBody)))
    }

    case MethodUpdate(obj,label,selfVar,_,newBody) => Core.Update(desugar(obj),label,Core.Method(selfVar,desugar(newBody)))

    case _ => sys.error("TODO: Fill me in!")
  }
}

// vim: set ts=2 sw=2 et sts=2:
