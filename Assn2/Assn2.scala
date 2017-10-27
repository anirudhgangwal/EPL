// Version 1.0

import scala.collection.immutable.Set

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object Assn2 {
  type Variable = String
  type Env[A] = Map[Variable,A]

  // Arithmetic expressions

  abstract class Expr
  case class Num(n: Integer) extends Expr
  case class Plus(e1: Expr, e2: Expr) extends Expr
  case class Minus(e1: Expr, e2: Expr) extends Expr
  case class Times(e1: Expr, e2: Expr) extends Expr

  // Booleans
  case class Bool(n: Boolean) extends Expr
  case class Eq(e1: Expr, e2:Expr) extends Expr
  case class IfThenElse(e: Expr, e1: Expr, e2: Expr) extends Expr
  
   // Strings
  case class Str(s: String) extends Expr
  case class Length(e: Expr) extends Expr
  case class Index(e1: Expr, e2: Expr) extends Expr
  case class Concat(e1: Expr, e2: Expr) extends Expr

  // Variables and let-binding
  case class Var(x: Variable) extends Expr
  case class Let(x: Variable, e1: Expr, e2: Expr) extends Expr
  case class LetFun(f: Variable, arg: Variable, ty: Type, e1:Expr, e2:Expr)
      extends Expr
  case class LetRec(f: Variable, arg: Variable, xty: Type, ty: Type, e1:Expr, e2:Expr)
      extends Expr
  case class LetPair(x: Variable,y: Variable, e1:Expr, e2:Expr) extends Expr

  // Pairing
  case class Pair(e1: Expr, e2: Expr) extends Expr
  case class First(e: Expr) extends Expr
  case class Second(e: Expr) extends Expr

  // Functions
  case class Lambda(x: Variable, ty: Type, e: Expr) extends Expr
  case class Apply(e1: Expr, e2: Expr) extends Expr
  case class Rec(f: Variable, x: Variable, tyx:Type, ty: Type, e: Expr) extends Expr

  // Values
  abstract class Value
  case class NumV(n: Integer) extends Value
  case class BoolV(n: Boolean) extends Value
  case class StringV(s: String) extends Value
  case class PairV(v1: Value, v2: Value) extends Value
  case class ClosureV(env: Env[Value], x: Variable, e: Expr) extends Value
  case class RecV(env: Env[Value], f:Variable, x: Variable, e: Expr) extends Value

  // Types
  abstract class Type
  case object IntTy extends Type
  case object BoolTy extends Type
  case object StringTy extends Type
  case class PairTy(ty1: Type, ty2: Type) extends Type
  case class FunTy(ty1: Type, ty2: Type) extends Type


  // ======================================================================
  // Part 1: Interpretation
  // ======================================================================

  // ======================================================================
  // Exercise 1: Primitive operations
  // ======================================================================

  
  object Value {
    // utility methods for operating on values
    def add(v1: Value, v2: Value): Value = (v1,v2) match {
      case (NumV(v1), NumV(v2)) => NumV (v1 + v2)
      case _ => sys.error("arguments to addition are non-numeric")
    }

    def subtract(v1: Value, v2: Value): Value = (v1,v2) match {
      case (NumV(v1), NumV(v2)) => NumV (v1 - v2)
      case _ => sys.error("arguments to addition are non-numeric")
    }

    def multiply(v1: Value, v2: Value): Value = (v1,v2) match {
      case (NumV(v1), NumV(v2)) => NumV(v1 * v2)
      case _ => sys.error("arguments to multiplication are non-numeric")
    }
     
    def eq(v1: Value, v2: Value): Value = (v1,v2) match {
      case (NumV(v1), NumV(v2)) => BoolV(v1 == v2)
      case (StringV(v1), StringV(v2)) => BoolV(v1 == v2)
      case (BoolV(v1), BoolV(v2)) => BoolV(v1 == v2)
      case _ => sys.error("arguments type do not match")
    }

    def length(v: Value): Value = (v) match {
      case (StringV(v)) => NumV(v.length())
      case _ => sys.error("expected argument type is string")
    }

    def index(v1: Value, v2: Value): Value = (v1,v2) match {
      case (StringV(v1), NumV(v2)) => StringV(v1.charAt(v2).toString())
      case _ => sys.error("index takes a string and integer")
    }

    def concat(v1: Value, v2: Value): Value =(v1,v2) match {
      case (StringV(v1),StringV(v2)) => StringV(v1 + v2)
      case _ => sys.error("Both arguments should b strings for concatenaion")
    }

  }

 

  
  // ======================================================================
  // Exercise 2: Evaluation
  // ======================================================================

  def eval (env: Env[Value], e: Expr): Value = e match {
    // Arithmetic
    case Num(n) => NumV(n)
    case Plus(e1,e2) => 
      Value.add(eval(env,e1),eval(env,e2))
    case Minus(e1,e2) => 
      Value.subtract(eval(env,e1),eval(env,e2))
    case Times(e1,e2) =>
      Value.multiply(eval(env,e1),eval(env,e2))
    
    // Booleans
    case Bool(b) => BoolV(b)
    case Eq(e1,e2) =>
      Value.eq(eval(env,e1),eval(env,e2))
    case IfThenElse(e,e1,e2) =>
      if (eval(env,e) == BoolV(true)) eval(env,e1) else eval(env,e2)
    
    // Strings
    case Str(s) => StringV(s)
    case Length(s) => Value.length(eval(env,s))
    case Index(e1,e2) => Value.index(eval(env,e1),eval(env,e2))
    case Concat(e1,e2) => Value.concat(eval(env,e1),eval(env,e2))
  
    // Variables and let-binding
    case Var(x) => env(x)
    case Let(x,e1,e2) => {
      eval(env + (x -> eval(env,e1)),e2)
    }
    
    //Pair
    case Pair(e1,e2) => PairV(eval(env,e1),eval(env,e2))
    case First(e) => eval(env,e) match {
      case PairV(e1,e2) => e1
    }
    case Second(e) => eval(env,e) match {
      case PairV(e1,e2) => e2
    }

    //Functions
    case Lambda(x,ty,e) => ClosureV(env,x,e)
    case Rec(f,x,tyx,ty,e) => RecV(env,f,x,e)
    case Apply(e1,e2) => (eval(env,e1),eval(env,e2)) match {
      case (ClosureV(env2,x,e),v2) => {
        eval(env2 + (x -> v2),e)
      }
      case (RecV(env2,f,x,e),v2) => {        
        eval(env2 + (f -> RecV(env2,f,x,e), x -> v2),e)
      }
    }

    case _ => sys.error("eval: Evaluation failed.")
  }


  // ======================================================================
  // Part 2: Typechecking
  // ======================================================================

  // ======================================================================
  // Exercise 3: Typechecker
  // ======================================================================

  // typing: calculate the return type of e, or throw an error
  def tyOf(ctx: Env[Type], e: Expr): Type = e match {
    // Arithmetic
    case Num(n) => IntTy
    case Plus(e1,e2) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
      case (IntTy, IntTy) => IntTy
      case _ => sys.error("non-integer arguments to +") 
    }
    case Minus(e1,e2) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
      case (IntTy, IntTy) => IntTy
      case _ => sys.error("non-integer arguments to -")
    }
    case Times(e1,e2) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
      case (IntTy, IntTy) => IntTy
      case _ => sys.error("non-integer arguments to *")
    }

    // Boolean
    case Bool(b) => BoolTy
    case Eq(e1,e2) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
      case (IntTy,IntTy) => BoolTy
      case (BoolTy,BoolTy) => BoolTy
      case (StringTy,StringTy) => BoolTy
      case _ => sys.error("given arguments to eq have different types") 
    }
    case IfThenElse(e,e1,e2) => (tyOf(ctx,e),tyOf(ctx,e1),tyOf(ctx,e2)) match {
      case (BoolTy,a: Type,b: Type) if a == b => a
      case (_,a: Type,b: Type) if a == b => sys.error("Non-boolean if statement")
      case (BoolTy,a: Type,b: Type) => sys.error("then and else have different return types")
      case _ => sys.error("Invalid IfThenElse statement")
    }

    // String
    case Str(s) => StringTy
    case Length(s) => tyOf(ctx,s) match {
      case StringTy => IntTy
      case _ => sys.error("length takes string argument")
    }
    case Index(e1,e2) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
      case (StringTy,IntTy) => StringTy
      case _ => sys.error("index takes a  string and integer")
    }
    case Concat(e1,e2) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
      case (StringTy,StringTy) => StringTy
      case _ => sys.error("concat requires two strings")
    }

    // Variables and let-binding
    case Var(x) => ctx(x)
    case Let(x,e1,e2) => tyOf(ctx + (x -> (tyOf(ctx,e1))), e2)

    // Pairs
    case Pair(e1,e2) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
      case (a:Type,b:Type) => PairTy(a,b)
      case _ => sys.error("invalid pair")
    }
    case First(e) => tyOf(ctx,e) match {
      case PairTy(a:Type,_) => a
      case _ => sys.error("First takes a pair as argument")
    }
    case Second(e) => tyOf(ctx,e) match {
      case PairTy(_,b:Type) => b
      case _ => sys.error("Second takes a pair as argument")
    }

    // function
    case Apply(e1,e2) => (tyOf(ctx,e1),tyOf(ctx,e2)) match {
      case (FunTy(a:Type,b:Type),c:Type) if c == a => b
      case _ => sys.error("invalid function or argument type") 
    }
    case Lambda(x,ty,e) => (ty,tyOf(ctx + (x->ty),e)) match {
      case (ty: Type,tye: Type) => FunTy(ty,tye)
      case _ => sys.error("invalid lambda function.") 
    }
    case Rec(f,x,tyx,ty,e) => (tyx,tyOf(ctx + (f -> FunTy(tyx,ty), x -> tyx),e)) match {
      case (tyx: Type,tye: Type) => FunTy(tyx,tye)
      case _ => sys.error("invalid recursive function")
    }
    case LetPair(x,y,e1,e2) => (tyOf(ctx,e1),e2) match {
      case (PairTy(a:Type,b:Type),e2:Expr) => (tyOf(ctx + (x->a,y->b),e2))
      case _ => sys.error("invalid let pair statement")
    }
    case LetFun(f,arg,ty,e1,e2) => (ty,tyOf(ctx+(arg->ty),e1),e2) match {
      case (tao1:Type,tao2:Type,e2:Expr) => tyOf(ctx + (f->FunTy(tao1,tao2)),e2)
      case _ => sys.error("invalid function declaration with let")
    }
    case LetRec(f,arg,xty,ty,e1,e2) => (xty,tyOf(ctx+(f->FunTy(xty,ty),arg->xty),e1),e2) match {
      case (tao1:Type,tao2:Type,e2:Expr) => tyOf(ctx+(f->FunTy(tao1,tao2)),e2)
      case _ => sys.error("invalid recursive function declaration with  let rec")
    }

    case _ => sys.error("type checking failed.")

  }


    // ======================================================================
  // Part 3: Syntactic transformation
  // ======================================================================

  // ======================================================================
  // Exercise 4: Capture-avoiding substitution
  // ======================================================================

  // This object provides a method to generate a "fresh" variable name
  object Gensym {
    private var id = 0
    def gensym(s: Variable): Variable = {
      val fresh_s = s + "_" + id
      id = id + 1
      fresh_s
    }
  }

  def swapVar(x: Variable, y: Variable, z: Variable): Variable =
    if (x == y) {
      z
    } else if (x == z) {
      y
    } else {
      x
    }

  def swap(e: Expr, y: Variable, z: Variable): Expr = {
    def go(e: Expr): Expr = e match {
      case Num(n) => Num(n)
      case Plus(t1,t2) => Plus(go(t1),go(t2))
      case Minus(t1,t2) => Minus(go(t1),go(t2))
      case Times(t1,t2) => Times(go(t1),go(t2))

      case Bool(b) => Bool(b)
      case Eq(t1,t2) => Eq(go(t1),go(t2))
      case IfThenElse(t,t1,t2) => IfThenElse(go(t),go(t1),go(t2))

      case Str(s) => Str(s)
      case Length(t) => Length(go(t))
      case Index(t1,t2) => Index(go(t1),go(t2))
      case Concat(t1,t2) => Concat(go(t1),go(t2))

      case Var(x) => Var(swapVar(x,y,z))
      case Let(x,t1,t2) => Let(swapVar(x,y,z),go(t1),go(t2))
      case LetFun(f,x,ty,t1,t2) => LetFun(swapVar(f,y,z),swapVar(x,y,z),ty,go(t1),go(t2))
      case LetRec(f,x,xty,ty,t1,t2) => LetRec(swapVar(f,y,z),swapVar(x,y,z),xty,ty,go(t1),go(t2))
      case LetPair(x1,x2,t1,t2) => LetPair(swapVar(x1,y,z),swapVar(x2,y,z),go(t1),go(t2))

      case Pair(t1,t2) => Pair(go(t1),go(t2))
      case First(t) => First(go(t))
      case Second(t) => Second(go(t))


      case Lambda(x,ty,t) => Lambda(swapVar(x,y,z),ty,go(t))
      case Apply(t1,t2) => Apply(go(t1),go(t2))
      case Rec(f,x,xty,ty,t) => Rec(swapVar(f,y,z), swapVar(x,y,z), xty,ty,go(t))

    }
    go(e)
  }

  def subst(e1:Expr, e2:Expr, x: Variable): Expr =
    e1 match {
      case Num(e) => Num(e)
      case Plus(t1,t2) => Plus(subst(t1,e2,x),subst(t2,e2,x))
      case Minus(t1,t2) => Minus(subst(t1,e2,x),subst(t2,e2,x))
      case Times(t1,t2) => Times(subst(t1,e2,x),subst(t2,e2,x))

      case IfThenElse(t,t1,t2) => IfThenElse(subst(t,e2,x),subst(t1,e2,x),subst(t2,e2,x))
      case Str(s) => Str(s)
      case Length(t) => Length(subst(t,e2,x))
      case Index(t1,t2) => Index(subst(t1,e2,x),subst(t2,e2,x))
      case Concat(t1,t2) => Concat(subst(t1,e2,x),subst(t2,e2,x)) 

      case Var(y) =>
        if (x == y) {
          e2
        } else {
          Var(y)
        }
      case Let(y,t1,t2) => {
        val z = Gensym.gensym(y);
        val fresh_t2 = swap(t2,y,z);
        Let(z,subst(t1,e2,x),subst(fresh_t2,e2,x))
      }

      case Bool(b) => Bool(b)
      case Eq(t1,t2) => Eq(subst(t1,e2,x),subst(t2,e2,x))
      
      case Pair(t1,t2) => Pair(subst(t1,e2,x),subst(t2,e2,x))
      case First(t) => First(subst(t,e2,x))
      case Second(t) => Second(subst(t,e2,x))

      case Apply(t1,t2) => Apply(subst(t1,e2,x),subst(t2,e2,x))
      case Lambda(y,ty,t) => {
        val z = Gensym.gensym(y)
        val fresh_t = swap(t,y,z)
        Lambda(z,ty,subst(fresh_t,e2,x)) 
      }
      case Rec(f,y,yty,ty,t) => {
        val z = Gensym.gensym(y)
        val g = Gensym.gensym(f)
        val fresh_t = swap(swap(t,y,z),f,g)
        Rec(g,z,yty,ty,subst(fresh_t,e2,x))
      }
       
      case LetPair(y1,y2,t1,t2) => {
        val z1 = Gensym.gensym(y1)
        val z2 = Gensym.gensym(y2)
        val fresh_t2 = swap(swap(t2,y1,z1),y2,z2)
        LetPair(z1,z2,subst(t1,e2,x),subst(fresh_t2,e2,x))  
      }
      case LetFun(f,y,ty,t1,t2) => {
        val z = Gensym.gensym(y)
        val g = Gensym.gensym(f)
        val fresh_t1 = swap(t1,y,z)
        val fresh_t2 = swap(t1,f,g)
        LetFun(g,z,ty,subst(fresh_t1,e2,x),subst(fresh_t2,e2,x))
      }
      case LetRec(f,y,yty,ty,t1,t2) => {
        val z = Gensym.gensym(y)
        val g = Gensym.gensym(f)
        val fresh_t1 = swap(swap(t1,f,g),y,z)
        val fresh_t2 = swap(t2,f,g)
        LetRec(g,z,yty,ty,subst(fresh_t1,e2,x),subst(fresh_t2,e2,x))
      }

      case _ => sys.error("subst failed")
    }


  // ======================================================================
  // Exercise 5: Desugaring let fun, let rec and let pair
  // ======================================================================

  def desugar(e: Expr): Expr = e match {

    case Num(n) => Num(n)
    case Plus(e1,e2) => Plus(desugar(e1),desugar(e2))
    case Minus(e1,e2) => Minus(desugar(e1),desugar(e2))
    case Times(e1,e2) => Times(desugar(e1),desugar(e2))

    case Bool(b) => Bool(b)
    case Eq(e1,e2) => Eq(desugar(e1),desugar(e2))
    case IfThenElse(e,e1,e2) => IfThenElse(desugar(e),desugar(e1),desugar(e2))

    case Str(s) => Str(s)
    case Length(e) => Length(desugar(e))
    case Index(e1,e2) => Index(desugar(e1),desugar(e2))
    case Concat(e1,e2) => Concat(desugar(e1),desugar(e2))    

    case Var(x) => Var(x)
    case Let(x,e1,e2) => Let(x,desugar(e1),desugar(e2))
    
    case Pair(e1,e2) => Pair(desugar(e1),desugar(e2))
    case First(e) => First(desugar(e))
    case Second(e) => Second(desugar(e))


    case Lambda(x,ty,e) => Lambda(x,ty,desugar(e))
    case Apply(e1,e2) => Apply(desugar(e1),desugar(e2))
    case Rec(f,x,xty,ty,e) => Rec(f, x, xty,ty,desugar(e))
    
    case LetPair(x,y,e1,e2) => {
      val p = Gensym.gensym("p")
      Let( p, e1, subst(subst(e2,First(e1),x),Second(e1),y)) 
    }
    case LetFun(f,y,ty,e1,e2) => Let(f,Lambda(y,ty,e1),e2)
    case LetRec(f,x,xty,ty,e1,e2) => Let(f,Rec(f,x,xty,ty,e1),e2)

    case _ => sys.error("desugar: failed")

  }


  // ======================================================================
  // Some simple programs
  // ======================================================================

  // The following examples illustrate how to embed Giraffe source code into
  // Scala using multi-line comments, and parse it using parser.parseStr.

  // Example 1: the swap function
  def example1: Expr = parser.parseStr("""
    let fun swap(x:int * int) = (snd(x), fst(x)) in 
    swap(42,17)
    """)

  // Example 2: the factorial function, yet again
  def example2: Expr = parser.parseStr("""
    let rec fact(n:int):int = 
      if (n == 0) then 1 else n * fact(n - 1) in 
    fact(5)
    """)

  // Example 3: exponentiation
  def example3: Expr = parser.parseStr("""
    let rec power(input: int * int):int =
          let (x,n) = input in
          if (n == 0) then 1 else
          x * power(x,n-1)
        in
        power(2,10)
    """)

  // Example 4: check whether two strings have the same last character
  def example4: Expr = parser.parseStr("""
    let fun sameLastChar(input: str * str) = 
      let (s1,s2) = input in 
      index(s1,length(s1)-1) == index(s2,length(s2)-1)
    in sameLastChar("abcz","abcdefghijklmnopqrstuvwxyz")
    """)

  // Example 5: String slice
  def example5: Expr = parser.parseStr("""
    let rec slice(input: str * (int * int)) : str = 
      let (s,p) = input in
      let (base,len) = p in
      if (len == 0) then ""
      else concat(index(s,base), slice (s,(base + 1, len - 1)))
    in slice("abcdefghijklmnopqrstuvwxyz", (10, 10))""")

  // Example 6: Integer comparison
  def example6: Expr = parser.parseStr("""
    let comment = "Assumes nonnegative arguments" in 
    let rec lessthanorequal(nm: int * int) : bool = 
      let (n,m) = nm in 
      if (n == 0) then true 
      else if (m == 0) then false
      else lessthanorequal(n-1,m-1)
    in (lessthanorequal (5,6), lessthanorequal(6,5))""")




  /*======================================================================
   The rest of this file is support code, which you should not (and do not
   need to) change.
   ====================================================================== */

  class GiraffeParser extends StandardTokenParsers with PackratParsers {

    type P[+A] = PackratParser[A]

    def parseStr(input: String): Expr = {
      phrase(expression)(new lexical.Scanner(input)) match {
        case Success(ast, _) => ast
        case e: NoSuccess => sys.error(e.msg)
      }
    }

    def parse(input: String): Expr = {
      val source = scala.io.Source.fromFile(input)
      val lines = try source.mkString finally source.close()
      parseStr(lines)
    }

    lexical.reserved += ("let", "in", "rec", "if", "then", "else",
      "int","str","bool","true","false","fst","snd","concat",
      "index","length","fun"
    )
    lexical.delimiters += ("=","*", "\\", "+", "-", "(", ")", "==", ":", ".",
      "->", ","
    )

    lazy val expression: P[Expr] =
      simpleExpr

    lazy val lambda: P[Expr] =
      ("\\" ~> ident) ~ (":" ~> typ) ~ ("." ~> expression) ^^ {
        case arg~ty~body => Lambda(arg,ty,body)
      }

    lazy val rec: P[Expr] =
      ("rec" ~> ident) ~
        ("(" ~> ident) ~ (":" ~> typ) ~ ((")" ~ ":") ~> typ) ~
        ("." ~> expression) ^^ {
          case recArg~funArg~funType~recType~body =>
            Rec(recArg,funArg,funType,recType,body)
        }

    lazy val ifExpr: P[Expr] =
      ("if" ~> expression) ~
        ("then" ~> expression) ~
        ("else" ~> expression) ^^ {
          case cond~e1~e2 => IfThenElse(cond,e1,e2)
        }

    lazy val letExpr: P[Expr] =
      ("let" ~> ident) ~ ("=" ~> expression) ~ ("in" ~> expression) ^^ {
        case binder~e1~e2 => Let(binder,e1,e2)
      }

    lazy val letFun: P[Expr] =
      ("let" ~ "fun" ~> ident) ~ ("(" ~> ident) ~
        (":" ~> typ <~ ")") ~ ("=" ~> expression) ~
        ("in" ~> expression) ^^ {
          case fun~binder~ty~e1~e2 => LetFun(fun,binder,ty,e1,e2)
        }

    lazy val letRec: P[Expr] =
      ("let" ~ "rec" ~> ident) ~ ("(" ~> ident) ~
        (":" ~> typ <~ ")") ~ (":" ~> typ) ~ ("=" ~> expression) ~
        ("in" ~> expression) ^^ {
          case fun~binder~xty~ty~e1~e2 => LetRec(fun,binder,xty,ty,e1,e2)
        }

    lazy val letPair: P[Expr] =
      ("let" ~ "(") ~> ident ~ ("," ~> ident <~ ")") ~
        ("=" ~> expression) ~ ("in" ~> expression) ^^ {
          case x~y~e1~e2 => LetPair(x,y,e1,e2)
        }

    lazy val typ: P[Type] =
      funTyp 

    lazy val funTyp: P[Type] =
      pairTyp ~ "->" ~ funTyp ^^ {
        case t1~"->"~t2 => FunTy(t1,t2)
      } | pairTyp

    lazy val pairTyp: P[Type] =
      primitiveType ~ "*" ~ pairTyp ^^ {
        case t1~"*"~t2 => PairTy(t1,t2)
      } | primitiveType

    lazy val primitiveType: P[Type] =
      "bool" ^^^ BoolTy | "int" ^^^ IntTy | "str" ^^^ StringTy |  "("~>typ<~")"

    lazy val operations: P[Expr] =
      application | 
      ("fst" ~ "(") ~> expression <~ ")" ^^ (x => First(x)) |
        ("snd" ~ "(") ~> expression <~ ")" ^^ (x => Second(x)) |
        ("length" ~ "(") ~> expression <~ ")" ^^ (x => Length(x)) |
        ("concat"  ~ "(") ~> expression ~ ("," ~> expression) <~ ")" ^^ {
          case e1~e2 => Concat(e1,e2)
        } |
        ("index" ~ "(") ~> expression ~ ("," ~> expression) <~ ")" ^^ {
          case e1~e2 => Index(e1,e2)
        }

    lazy val arith: P[Expr] =
      eq

    lazy val prod: P[Expr] =
      prod ~ "*" ~ fact ^^ {
        case e1~"*"~e2 => Times(e1,e2)
      } | fact

    lazy val summation: P[Expr] =
      summation ~ "+" ~ prod ^^ {
        case e1~"+"~e2 => Plus(e1,e2)
      } | summation ~ "-" ~ prod ^^ {
        case e1~"-"~e2 => Minus(e1,e2)
      } | prod

    lazy val eq: P[Expr] =
      simpleExpr ~ "==" ~ summation ^^ {
        case e1~"=="~e2 => Eq(e1,e2)
      } | summation

    lazy val application: P[Expr] =
      fact ~ fact ^^ {
        case e1~e2 => Apply(e1,e2)
      }

    lazy val simpleExpr: P[Expr] = (
      lambda |
        rec |
        letExpr |
        letFun |
        letRec |
        letPair |
        ifExpr |
        arith |
        fact
    )

    lazy val pairLit: P[Expr] =
      "(" ~> expression ~ "," ~ expression <~ ")" ^^ {
        case t1~","~t2 => Pair(t1,t2)
      }

    lazy val fact: P[Expr] = (
      operations |
        pairLit |
        (ident ^^ Var) |
        (numericLit ^^ { x => Num(x.toInt) }) |
        (stringLit ^^ Str) |
        ("true" ^^^ Bool(true)) |
        ("false" ^^^ Bool(false)) |
        "("~>expression<~")"
    )

  }


  val parser = new GiraffeParser

  
  object Main {
    def typecheck(ast: Expr):Type =
      tyOf(Map.empty,ast);

    def evaluate(ast: Expr):Value =
      eval(Map.empty,ast)



    def showResult(ast: Expr) {
      println("AST:  " + ast.toString + "\n")

      try {
        print("Type Checking...");
        val ty = typecheck(ast);
        println("Done!");
        println("Type of Expression: " + ty.toString + "\n") ;
      } catch {
          case e:Throwable => println("Error: " + e)
      }
      try {
        println("Desugaring...");
        val core_ast = desugar(ast);
        println("Done!");
        println("Desugared AST: " + core_ast.toString + "\n") ;

        println("Evaluating...");
        println("Result: " + evaluate(core_ast))
      } catch {
        case e:Throwable => {
          println("Error: " + e)
          println("Evaluating raw AST...");
          println("Result: " + evaluate(ast))
        }
      }
     
    }

    def start(): Unit = {
      println("Welcome to Giraffe! (V1.0, October 6, 2017)");
      println("Enter expressions to evaluate, :load <filename.gir> to load a file, or :quit to quit.");
      println("This REPL can only read one line at a time, use :load to load larger expressions.");
      repl()
    }

    def repl(): Unit = {
      print("Giraffe> ");
      val input = scala.io.StdIn.readLine();
      if(input == ":quit") {
        println("Goodbye!")
      }
      else if (input.startsWith(":load")) {
        try {
          val ast = parser.parse(input.substring(6));
          showResult(ast)
        } catch {
          case e:Throwable => println("Error: " + e)
        }
        repl()
      } else {
        try {
          val ast = parser.parseStr(input);
          showResult(ast)
        } catch {
          case e:Throwable => println("Error: " + e)
        }
        repl()
      }
    }

  }
  def main( args:Array[String] ):Unit = {
    if(args.length == 0) {
      Main.start()
    } else {
      try {
        print("Parsing...");
        val ast = parser.parse(args.head)
        println("Done!");
        Main.showResult(ast)
      } catch {
        case e:Throwable => println("Error: " + e)
      }
    }
  }
}


