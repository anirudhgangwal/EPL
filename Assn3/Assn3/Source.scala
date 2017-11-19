package Assignment3.Source
import scala.collection.immutable.ListMap
import Assignment3.Utility.Utility._
import Assignment3.SourceHelpers.SourceHelpers._
// Source language
object Source {
  type TermVariable = Variable
  type TypeVariable = Variable
  type Label = String
  type ClassName = String
  type ObjectName = String

  // - Types
  abstract class Type
  case class TyVar(x: TypeVariable) extends Type
  case object TyTop extends Type
  case class TyObject(bnd: TypeVariable, fields: ListMap[Label, Type]) extends Type
  case class TyClass(ty: Type) extends Type
  case object TyBool extends Type
  case object TyInt extends Type
  case object TyString extends Type
  case class TyFun(args: List[Type], res: Type) extends Type

  // - Expressions
  abstract class Expr
  case class Var(name: TermVariable) extends Expr

  case class Func(params: List[(Variable, Type)], body: Expr) extends Expr
  case class Apply(e: Expr, args: List[Expr]) extends Expr

  // -- Object-oriented features
  case class SelectField(obj: Expr, methodLabel: Label) extends Expr
  case class MethodUpdate(
    obj: Expr,
    methodLabel: Label,
    selfVar: Variable,
    selfTy: Type,
    newBody: Expr) extends Expr
  case class FieldUpdate(obj: Expr, fieldLabel: Label, newBody: Expr) extends Expr
  case class New(c: Expr) extends Expr

  // Root
  case object RootClass extends Expr

  // --- Type synonym
  case class TypeIn(tv: TypeVariable, ty: Type, e: Expr) extends Expr

  // --- Let binding
  case class LetIn(x: TermVariable, e1: Expr, e2: Expr) extends Expr

  // --- Object definition
  case class Object(
    selfBinder: TypeVariable,                 // Name of the self binder
    selfTy: Type,                   // Type of the object
    fields: ListMap[Label, Expr])             // Fields / Methods of the object
  extends Expr
  // --- Class definition
  case class Class(
    selfBinder: Variable,                                // Name of the self binder
    selfType: Type,                            // Type annotation on the self binder
    extendsData: Option[(Expr, Type)],     // Superclass, type
    fields: ListMap[Label, Expr],                        // Map from labels to methods, _not overridden_
    overriddenMethods: ListMap[Label, Expr]              // Map from labels to methods, where labels occur in superclass
  ) extends Expr

  // -- Basic terms
  case class IfThenElse(e1: Expr, e2: Expr, e3: Expr) extends Expr
  case class Bool(b: Boolean) extends Expr
  case class Num(n: Integer) extends Expr
  case class Str(str: String) extends Expr
  case class BinOp(op: BinaryOp, e1: Expr, e2: Expr) extends Expr
  case class NotOp(e: Expr) extends Expr


  abstract class BinaryOp
  case object EqOp extends BinaryOp
  case object AddOp extends BinaryOp
  case object SubOp extends BinaryOp
  case object MulOp extends BinaryOp
  case object DivOp extends BinaryOp
  case object AndOp extends BinaryOp
  case object OrOp extends BinaryOp

  // - Typechecking

  type TermVarEnv = ListMap[Variable, Type]


  // Typechecks a program (closed expression)
  def typecheckProgram(term: Expr): Type = {
    typeCheck(new ListMap(): TermVarEnv, term)
  }

  def typeCheck(termEnv: TermVarEnv, term: Expr): Type = {

    // Helper definitions
    /*
    * Helper definitions -- remember we have provided:
    *  - subtypeOf: (Type, Type) => Boolean
    *  - typeSwap: (Type, TypeVariable, TypeVariable) => Type
    *  - typeSubst: (Type, Type, TypeVariable) => Type
    *  - equivTypes: (Type, Type) => Boolean
    */

    // tc checks a term under the current environment
    def tc(e: Expr): Type = typeCheck(termEnv, e)

    // assertTy typechecks a term, and throws an error if it doesn't have
    // the given type.
    def assertTy(term: Expr, asserted: Type): Unit = {
      val termTy = tc(term);
      if(equivTypes(termTy, asserted)) { () } else {
        sys.error("Got type " + termTy.toString +
          ", but expected type " + asserted.toString)
      }
    }


    term match {
      case TypeIn(_, _, _) =>
        sys.error("INTERNAL ERROR! TypeIn SHOULD NOT APPEAR. Please report this!")
      // Exercise 2 
      case Var(name) => termEnv(name)     
      case Func(params,body) => {
        var env = new scala.collection.mutable.ListMap[Variable, Type]()
        var args = new scala.collection.mutable.MutableList[Type]()
        env ++= termEnv
        for ((variable,t) <- params) {
          env(variable) = t
          args += t
        }
        val listMap = ListMap(env.toList : _*)
        val list =  args.toList
        TyFun(list,typeCheck(listMap,body))
      }   
      case Apply(e,args) => tc(e) match {
        case TyFun(argTypes,res) => {
          if (args.length == argTypes.length) {
            for (i <- 0 to args.length-1) {
              if (subtypeOf(tc(args(i)),argTypes(i))) {} else {sys.error("Func arguments mismatch.")}
            }
          } else {
            sys.error("Number of function arguments and actual arguments given differ.")
          }
          return res 
        } 
      }
      case LetIn(x,e1,e2) => typeCheck(termEnv + (x->tc(e1)),e2)        
      case IfThenElse(e1,e2,e3) => (tc(e1),tc(e2),tc(e3)) match {
        case (TyBool,t2,t3) => if (equivTypes(t2,t3)) {t2} else {sys.error("If - type mismatch.")}
      }
      case Bool(b) => TyBool
      case Num(n) => TyInt
      case Str(s) => TyString
      case BinOp(EqOp,e1,e2) => (tc(e1),tc(e2)) match {
        case (TyInt,TyInt) => TyBool
        case (TyBool,TyBool) => TyBool
        case (TyString,TyString) => TyBool
      }
      case BinOp(op,e1,e2) => (op,tc(e1),tc(e2)) match {
        case (AddOp,TyInt,TyInt) => TyInt
        case (SubOp,TyInt,TyInt) => TyInt
        case (MulOp,TyInt,TyInt) => TyInt
        case (DivOp,TyInt,TyInt) => TyInt
        case (AndOp,TyBool,TyBool) => TyBool
        case (OrOp,TyBool,TyBool) => TyBool
        case _ => sys.error("Invalid binary operation.")      
      }
      case NotOp(e) => if (tc(e)==TyBool) {TyBool} else {sys.error("Not operation with non-bool.")}
      
      // Exercise 3      
      case Object(selfBinder,selfTy,fields) => selfTy match {
        case TyObject(bnd,fieldsTy) => {
          if (fields.keys.size == fieldsTy.keys.size) {
            for ((label,tpe) <- fieldsTy) {
              if (fields.contains(label)) {
                if (equivTypes(typeCheck(termEnv + (selfBinder -> selfTy),fields(label)),typeSubst(tpe,selfTy,bnd))) {
                  // Do nothing.
                } else {
                  sys.error("Type mismatch b/w Object type and actual object given.")
                }               
              } else {
                sys.error("Object does not contain label defined in its type.")
              }
            }
          } else {
            sys.error("Object type mismatch. Number of fields different.")
          }
        } 
        selfTy  // return selfTy if no errors found above.
      }
      
      case SelectField(obj,methodLabel) => tc(obj) match {
        case TyObject(bnd,fields) => {
          if (fields.contains(methodLabel)) {
            typeSubst(fields(methodLabel),tc(obj),bnd)
          } else {
            sys.error("Label not present in object's fields. ")
          }
        }
      }

      case FieldUpdate(obj,fieldLabel,newBody) => tc(obj) match {
        case TyObject(bnd,fields) => {
          if (fields.contains(fieldLabel)) {
            if (equivTypes(tc(newBody),typeSubst(fields(fieldLabel),tc(obj),bnd))) {
              tc(obj)
            } else {
              sys.error("Type of new body for given label not correct.")
            }
          } else {
            sys.error("Label not present in object.")
          }
        }
      }

      case MethodUpdate(obj,methodLabel,selfVar,selfTy,newBody) => tc(obj) match {
        case TyObject(bnd,fields) => {
          if (equivTypes(tc(obj),selfTy)) {
            if (fields.contains(methodLabel)) {
              if (equivTypes(typeCheck(termEnv + (selfVar -> tc(obj)),newBody),typeSubst(fields(methodLabel),tc(obj),bnd))) {
                 tc(obj)
              } else {
                sys.error("Type of new body for given label not correct.") 
              } 
            } else {
              sys.error("Label not present in object.")
            }
          } else {
            sys.error("Object type and self type of method to update are of different types.")
          }
        }
      }

      // Exercise 4
      case New(e) => tc(e) match {
        case TyClass(ty) => ty match {
          TyObject(bnd,fields) => ty
        }
      }
      
      

      case _ => sys.error("TODO -- fill me in!")
    }
  }
}
//class MethodUpdate(
//    obj: Expr,
//    methodLabel: Label,
//    selfVar: Variable,
//    selfTy: Type,
//    newBody: Expr)

// class FieldUpdate(obj: Expr, fieldLabel: Label, newBody: Expr)
// class SelectField(obj: Expr, methodLabel: Label)
// TyObject(bnd: TypeVariable, fields: ListMap[Label, Type])
// case class Object(
   // selfBinder: TypeVariable,                 // Name of the self binder
   // selfTy: Type,                   // Type of the object
   // fields: ListMap[Label, Expr]) 
 
      // TyFun(args: List[Type], res: Type) extends Type
      // Func(params: List[(Variable, Type)], body: Expr)
      // Apply(e: Expr, args: List[Expr]) 
      // LetIn(x: TermVariable, e1: Expr, e2: Expr) 


// vim: set ts=2 sw=2 et sts=2:
