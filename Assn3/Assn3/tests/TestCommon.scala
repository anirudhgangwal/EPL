//import org.scalatest._
//import scala.collection.immutable.ListMap
package Assignment3Tests.TestCommon
import Assignment3.Assn3.Assn3._;

object TestCoreHelpers {
  import Assignment3.Core.Core._

  val coreParser = new ParseCore.CoreParser

  def parseStr(str: String): Expr = {
    coreParser.parseStr(str)
  }

  def aequiv(e1: Expr, e2: Expr): Boolean = {
    def equivVar(l: List[(Variable,Variable)], v1: Variable, v2: Variable): Boolean = l match {
      case Nil => v1 == v2
      case (x1,x2)::xs => {
        if (v1 == x1 || v2 == x2) {
          v1 == x1 && v2 == x2
        } else {
          equivVar(xs,v1,v2)
        }
      }
    }

    def go(vs: List[(Variable, Variable)], e1:Expr, e2: Expr): Boolean = (e1, e2) match {
      case (v1: Value, v2: Value) => v1 == v2
      case (Var(v1), Var(v2)) => equivVar(vs, v1, v2)
      case (Apply(e11, e12), Apply(e21, e22)) =>
        go(vs, e11, e21) && go(vs, e12, e22)
      case (IfThenElse(e11, e12, e13), IfThenElse(e21, e22, e23)) =>
        go(vs, e11, e21) && go(vs, e12, e22) && go(vs, e13, e23)
      case (BinOp(op1, e11, e12), BinOp(op2, e21, e22)) =>
        (op1 == op2) && go(vs, e11, e21) && go(vs, e12, e22)
      case (NotOp(e1), NotOp(e2)) => go(vs, e1, e2)
      case (Invoke(e1, l1), Invoke(e2, l2)) => (l1 == l2) && go(vs, e1, e2)
      case (LetIn(x1, e11, e12), LetIn(x2, e21, e22)) =>
        go(vs, e11, e21) && go((x1, x2)::vs, e12, e22)
      case (Lambda(x1, e1), Lambda(x2, e2)) => go((x1, x2)::vs, e1, e2)
      case (Object(meths1), Object(meths2)) => {
        if (meths1.keySet != meths2.keySet) { false } else {
          meths1.forall( (entry: (Label, Method)) => {
              val (lbl, Method(bnd1, body1)) = entry;
              val Method(bnd2, body2) = meths2(lbl);
              go((bnd1, bnd2)::vs, body1, body2)
          })
        }
      }
      case (Update(o1, lbl1, Method(bnd1, body1)),
           Update(o2, lbl2, Method(bnd2, body2))) => {
        go(vs, o1, o2) && (lbl1 == lbl2) && go((bnd1, bnd2)::vs, body1, body2)
      }
      case (_, _) => false
    }

    go(List(): List[(Variable, Variable)], e1, e2)
  }

  def assertEquiv(e1: Expr, e2: Expr) = {
    assert(aequiv(e1, e2), "Error: expected " + e2.toString + " but got " + e1.toString)
  }
}


object TestSourceHelpers {
  import Assignment3.Source._
  import Assignment3.SourceHelpers._
  val sugaredParser = new ParseSource.SourceParser

  def parseStr(sugaredStr: String): Source.Expr = {
    SourceHelpers.resolveTypeAliases(sugaredParser.parseStr(sugaredStr))
  }
}

// vim: set ts=2 sw=2 et sts=2:
