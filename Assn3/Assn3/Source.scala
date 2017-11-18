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
      case _ => sys.error("TODO -- fill me in!")
    }
  }
}



// vim: set ts=2 sw=2 et sts=2:
