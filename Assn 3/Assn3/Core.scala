package Assignment3.Core
import scala.collection.immutable.ListMap
import Assignment3.Utility.Utility._

// Core object calculus
object Core {

  //// Definition
  type Variable = String
  type Label = String
  case class Method(selfBinder: Variable, body: Expr)

  abstract class Expr
  case class Var(name: Variable) extends Expr
  case class Object(methods: ListMap[Label, Method]) extends Expr
  case class Lambda(x: Variable, body: Expr) extends Expr
  case class Apply(f: Expr, arg: Expr) extends Expr
  case class LetIn(v: Variable, e1: Expr, e2: Expr) extends Expr
  case class Invoke(obj: Expr, methodLabel: Label) extends Expr
  case class Update(obj: Expr, methodLabel: Label, newMethod: Method) extends Expr


  // -- Basic terms
  case class IfThenElse(e1: Expr, e2: Expr, e3: Expr) extends Expr
  case class BinOp(op: BinaryOp, e1: Expr, e2: Expr) extends Expr
  case class NotOp(e: Expr) extends Expr


  abstract class BinaryOp
  case object AddOp extends BinaryOp
  case object SubOp extends BinaryOp
  case object MulOp extends BinaryOp
  case object DivOp extends BinaryOp
  case object AndOp extends BinaryOp
  case object OrOp extends BinaryOp
  case object EqOp extends BinaryOp

  abstract class Value extends Expr
  case class ObjectV(methods: ListMap[Variable, Method]) extends Value
  case class LambdaV(x: Variable, body: Expr) extends Value
  case class NumV(n: Integer) extends Value
  case class BoolV(b: Boolean) extends Value
  case class StrV(s: String) extends Value
  //// Evaluation

  // Swapping and substitution


  def swap(e: Expr, y: Variable, z: Variable): Expr = {

    def swapMethod(m: Method): Method = m match {
      case Method(x, body) =>
        Method(swapVar(x, y, z), swap(body, y, z))
    }

    def go(e: Expr): Expr = e match {
      case v: Value => v // Values are closed.
      case Var(x) => Var(swapVar(x, y, z))
      case Object(methods) => {
        val updatedMethods: ListMap[Label, Method] = methods.map((entry: (Label, Method)) => {
            val (label, body) = entry;
            (label, swapMethod(body))
        })
        Object(updatedMethods)
      }
      case Invoke(obj, methodLabel) => Invoke(go(obj), methodLabel)
      case Update(obj, methodLabel, method) => {
        Update(go(obj), methodLabel, swapMethod(method))
      }
      case Lambda(x, e) => {
        Lambda(swapVar(x, y, z), go(e))
      }
      case Apply(e1, e2) => Apply(go(e1), go(e2))
      case LetIn(x, e1, e2) => LetIn(swapVar(x, y, z), go(e1), go(e2))
      case IfThenElse(e1, e2, e3) => IfThenElse(go(e1), go(e2), go(e3))
      case BinOp(op, e1, e2) => BinOp(op, go(e1), go(e2))
      case NotOp(e) => NotOp(go(e))
      case e => e
    }

    go(e)
  }

  def subst(e1: Expr, e2: Expr, x: Variable): Expr = {
    def go(e: Expr): Expr = {

      e match {
        // Values are closed -- substitution has no effect
        case v: Value => v
        case Var(y) => 
          if (x == y) {
            e2
          } else {
            Var(y)
          }
        case BinOp(op,t1,t2) => BinOp(op,subst(t1,e2,x),subst(t2,e2,x))
        case IfThenElse(t1,t2,t3) => IfThenElse(subst(t1,e2,x),subst(t2,e2,x),subst(t3,e2,x))
        case NotOp(t) => NotOp(subst(t,e2,x))
        case LetIn(y,t1,t2) => {
          val z = Gensym.gensym(y)
          
        }  
        case _ => sys.error("TODO -- fill me in!")
      }
    }

    go(e1)
  }

  // Evaluation of the core calculus
  def eval(expr: Expr): Value = expr match {
    case v: Value => v
    case Var(_) => sys.error("Cannot evaluate a variable -- this should have been substituted!")
    case _ => sys.error("TODO -- fill me in!")
  }
}

// vim: set ts=2 sw=2 et sts=2:
