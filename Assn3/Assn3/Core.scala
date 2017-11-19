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
          val fresh_t2 = swap(t2,y,z)
          LetIn(z,subst(t1,e2,x),subst(fresh_t2,e2,x))
        }
        case Lambda(y,t) => {
          val z = Gensym.gensym(y)
          val fresh_t = swap(t,y,z)
          Lambda(z,subst(fresh_t,e2,x))        
        }
        case Apply(t1,t2) => Apply(subst(t1,e2,x),subst(t2,e2,x))
        case Object(methods) => {
          var new_methods = new scala.collection.mutable.ListMap[Label, Method]()
          for ((label,method) <- methods) {
            (label,method) match {
              case (label:Label,Method(selfbinder,body)) => {
                val z = Gensym.gensym(selfbinder)
                val fresh_body = swap(body,selfbinder,z)
                new_methods(label) = Method(z,subst(fresh_body,e2,x))
              }
            }
          }
          val listMap = ListMap(new_methods.toList : _*)
          Object(listMap)
        }
        case Invoke(obj,methodlabel) => Invoke(subst(obj,e2,x),methodlabel)
        case Update(obj,methodLabel,Method(selfbinder,body)) => {
          val z = Gensym.gensym(selfbinder)
          val fresh_body = swap(body,selfbinder,z)
          Update(subst(obj,e2,x),methodLabel,Method(z,subst(fresh_body,e2,x)))
        }
          
        case _ => sys.error("Substitution Failed.")
      }
    }

    go(e1)
  }

  // Evaluation of the core calculus
  def eval(expr: Expr): Value = expr match {
    case v: Value => v
    case Var(_) => sys.error("Cannot evaluate a variable -- this should have been substituted!")
    case LetIn(x,e1,e2) => eval(subst(e2,eval(e1),x)) // outer most eval necessary?
    case Lambda(f,arg) => LambdaV(f,arg)
    case Apply(f,arg) => eval(f) match {     
      case LambdaV(x,e) => eval(subst(e,eval(arg),x))
    }
    case BinOp(EqOp,e1,e2) => (eval(e1),eval(e2)) match {
      case (NumV(v1),NumV(v2)) => if (v1==v2) BoolV(true) else BoolV(false)
      case (BoolV(v1),BoolV(v2)) => if (v1==v2) BoolV(true) else BoolV(false)
      case (StrV(v1),StrV(v2)) => if (v1==v2) BoolV(true) else BoolV(false)
    }
    case IfThenElse(e,e1,e2) => if (eval(e)==BoolV(true)) eval(e1) else eval(e2)
    case BinOp(op,e1,e2) => (op,eval(e1),eval(e2)) match {
      case (AddOp,NumV(v1),NumV(v2)) => NumV(v1+v2)
      case (SubOp,NumV(v1),NumV(v2)) => NumV(v1-v2)
      case (MulOp,NumV(v1),NumV(v2)) => NumV(v1*v2)
      case (DivOp,NumV(v1),NumV(v2)) => NumV(v1/v2)
      case (AndOp,BoolV(v1),BoolV(v2)) => BoolV(v1 && v2)
      case (OrOp,BoolV(v1),BoolV(v2)) => BoolV(v1 || v2)
    }
    case NotOp(e) => eval(e) match {
      case BoolV(v) => BoolV(!v)
    }
    case Object(methods) => ObjectV(methods)
    case Invoke(obj,lj) => eval(obj) match {
      case ObjectV(methods) => if (methods contains lj) {
        methods(lj) match {
          case Method(selfbinder,body) => eval(subst(body,eval(obj),selfbinder))        
        }
      } else {sys.error("Label not in object.")}
    }
    case Update(obj,methodlabel,newmethod) => eval(obj) match {
      case ObjectV(methods) => {
        val v2 = methods + (methodlabel -> newmethod)
        ObjectV(v2)
      }
    }
    
    
    case _ => sys.error("Evaluation failed.")
  }
}

// vim: set ts=2 sw=2 et sts=2:
