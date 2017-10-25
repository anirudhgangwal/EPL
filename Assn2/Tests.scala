// Version 1.10

import Assn2._

def aequiv(e1: Expr, e2:Expr): Boolean = {
  def equivVar(l: List[(Variable,Variable)], v1: Variable, v2: Variable): Boolean = l match {
    case Nil => v1 == v2
    case (x1,x2)::xs =>
      if (v1 == x1 || v2 == x2) {
        v1 == x1 && v2 == x2
      } else {
        equivVar(xs,v1,v2)
      }
  };
  def go(l: List[(Variable,Variable)], e1: Expr, e2: Expr): Boolean = (e1,e2) match {
    case (Num(n), Num(m)) => n == m
    case (Plus(e11,e12), Plus(e21,e22)) =>
      go(l,e11,e21) && go(l,e12,e22)
    case (Times(e11,e12), Times(e21,e22)) =>
      go(l,e11,e21) && go(l,e12,e22)
    case (Minus(e11,e12), Minus(e21,e22)) =>
      go(l,e11,e21) && go(l,e12,e22)

    case (Bool(b1), Bool(b2)) => b1 == b2
    case (Eq(e11,e12), Eq(e21,e22)) =>
      go(l,e11,e21) && go(l,e12,e22)
    case (IfThenElse(e10,e11,e12), IfThenElse(e20,e21,e22)) =>
      go(l,e10,e20) && go(l,e11,e21) && go(l,e12,e22)

    case (Str(s1), Str(s2)) => s1 == s2
    case (Length(e1), Length(e2)) =>
      go(l,e1,e2) 
    case (Index(e11,e12), Index(e21,e22)) =>
      go(l,e11,e21) && go(l,e12,e22)
    case (Concat(e11,e12), Concat(e21,e22)) =>
      go(l,e11,e21) && go(l,e12,e22)

    case (Var(v1),Var(v2)) => equivVar(l,v1,v2)
    case (Let(x1,e11,e12),Let(x2,e21,e22)) =>
      go(l,e11,e21) && go((x1,x2)::l,e12,e22)
    case (LetFun(f1,x1,ty1,e11,e12),LetFun(f2,x2,ty2,e21,e22)) =>
      ty1 == ty2 && go((x1,x2)::l,e11,e21) && go((f1,f2)::l,e12,e22)
    case (LetRec(f1,x1,ty11,ty12,e11,e12),LetRec(f2,x2,ty21,ty22,e21,e22)) =>
      ty11 == ty21 && ty12 == ty22 &&
        go((f1,f2)::(x1,x2)::l,e11,e21) && go((f1,f2)::l,e12,e22)
    case (LetPair(x1,y1,e11,e12), LetPair(x2,y2,e21,e22)) =>
      go(l,e11,e21) && go((x1,x2)::(y1,y2)::l, e12,e22)

    case (Pair(e11,e12), Pair(e21,e22)) =>
      go(l,e11,e21) && go(l,e12,e22)
    case (First(e1), First(e2)) =>
      go(l,e1,e2)
    case (Second(e1), Second(e2)) =>
      go(l,e1,e2)
    
    case (Lambda(x1,t1,e1),Lambda(x2,t2,e2)) =>
      t1 == t2 && go((x1,x2)::l,e1,e2)
    case (Apply(e11,e12), Apply(e21,e22)) =>
      go(l,e11,e21) && go(l,e12,e22)
    case (Rec(f1,x1,ty11,ty12,e1),Rec(f2,x2,ty21,ty22,e2)) =>
      ty11 == ty21 && ty12 == ty22 &&
        go((f1,f2)::(x1,x2)::l,e1,e2)
    case (_,_) => false
  };
  go(Nil,e1,e2)
}

val substExp1: Boolean = aequiv(
  subst(
    parser.parseStr("""
        (\y:int.x + y) x
       """), parser.parseStr("(y+y)"),
    "x"),
  parser.parseStr("""
        (\z:int.(y + y) + z) (y+y)
       """))


val substExp2: Boolean = aequiv(
    subst(
      parser.parseStr("""
        let y = x in 
        x + y
       """), parser.parseStr("(y+y)"),
      "x"),
      parser.parseStr("""
        let z = (y + y) in (y + y) + z
       """))

val substExp3: Boolean =
  aequiv(
    subst(
      parser.parseStr("""
        (rec f (y: int): int.
          if (y == 0) then
            y else
            x + f(y - 1))
        y
       """),
      parser.parseStr("f y"),
      "x"),
    parser.parseStr("""
        (rec g (z: int): int.
          if (z == 0) then
            z else
            (f y) + g(z - 1))
        y
       """))

val substExp4: Boolean = aequiv(
  subst(
    desugar(parser.parseStr("""
        let (y,z) = (x,x+1) in 
        x + y + z
       """)), parser.parseStr("(y*z)"),
    "x"),
    desugar(parser.parseStr("""
        let (a,b) = ((y*z),(y*z)+1) in 
        (y*z) + a + b
       """)))

val substExp5: Boolean = aequiv(
  subst(
    desugar(parser.parseStr("""
        let fun f(y:int) = x + y in f x
       """)), parser.parseStr("(y+y)"),
    "x"),
     desugar( parser.parseStr("""
        let fun f(z:int) = (y+y) + z in f (y+y)
       """)))

val substExp6: Boolean =
  aequiv(
    subst(
      desugar(parser.parseStr("""
        let rec f (y: int): int = 
          if (y == 0) then
            y else
            x + f(y - 1) 
        in f y
       """)),
      parser.parseStr("f y"),
      "x"),
    desugar(parser.parseStr("""
        let rec g (z: int): int = 
          if (z == 0) then
            z else
            (f y) + g(z - 1) 
        in g y
       """)))
    


val substExp7: Boolean =
  eval(Map(),
    subst(desugar(parser.parseStr("""let rec f(x:int):int = x+1 in f 12""")),Num(14),"x")) == NumV(13)


val substExp8: Boolean =
  eval(Map(),
    subst(desugar(parser.parseStr("""let rec x(y:int):int = if y == 12 then x (y+1) else (y+1) in x 12""")),Num(26),"x")) == NumV(14)

val substExp9: Boolean =
  aequiv(subst(Rec("f","x",IntTy,IntTy,Plus(Num(2),Var("x"))),Num(20),"x"), Rec("f","x",IntTy,IntTy,Plus(Num(2),Var("x"))))


val substExp10: Boolean =
  eval(Map(),
    subst(desugar(parser.parseStr("""let (a,b) = (12,13) in (let rec f(x:int):int = x+1 in f a)""")),Num(14),"x")) == NumV(13)

val substExp11: Boolean =
  eval(Map(),
    subst(desugar(parser.parseStr("""let (a,b) = (12,13) in (let fun f(x:int) = x+1 in f a)""")),Num(14),"x")) == NumV(13)

val substExp12: Boolean =
  eval(Map(),
    subst(desugar(parser.parseStr("""let (a,b) = (12,13) in (let rec f(x:int):int = x+1 in f a)""")),Num(14),"x")) == NumV(13)

