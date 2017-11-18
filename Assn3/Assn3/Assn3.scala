package Assignment3.Assn3
import scala.collection.immutable.ListMap
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

// This is all support code. You shouldn't need to touch this :)

object Assn3 {

  // Parsing of Core language
  object ParseCore {

    class CoreParser extends StandardTokenParsers with PackratParsers {
      import Assignment3.Core.Core._


      def parseStr(input: String): Expr = {
        phrase(expression)(new lexical.Scanner(input)) match {
          case Success(ast, _) => ast
          case e: NoSuccess => sys.error(e.toString)
        }
      }

      def parse(input: String): Expr = {
        val source = scala.io.Source.fromFile(input)
        val lines = try source.mkString finally source.close()
        parseStr(lines)
      }

      lexical.reserved += ("let", "in", "if", "then", "else", "true", "false", "update", "with")
      lexical.delimiters += ("=", ".", "==",
        "=>", "(", ")", "+", "-", "*", "/", "&&", "||", "\\", ";", "[", "]", "<=", "!", "::")

      type P[+A] = PackratParser[A]

      lazy val binaryOp: P[Expr] = {
        binRel
      }

      lazy val binRel: P[Expr] =
        expression ~ "==" ~ summation ^^ {
          case e1~_~e2 => BinOp(EqOp, e1,e2)
        } | expression ~ "&&" ~ summation ^^ {
          case e1~_~e2 => BinOp(AndOp, e1,e2)
        } | expression ~ "||" ~ summation ^^ {
          case e1~_~e2 => BinOp(OrOp, e1,e2)
        } | summation

      lazy val summation: P[Expr] =
        summation ~ "+" ~ prod ^^ {
          case e1~"+"~e2 => BinOp(AddOp, e1, e2)
        } | summation ~ "-" ~ prod ^^ {
          case e1~"-"~e2 => BinOp(SubOp, e1, e2)
        } | prod

      lazy val prod: P[Expr] =
        prod ~ "*" ~ unaryOp ^^ {
          case e1~"*"~e2 => BinOp(MulOp, e1,e2)
        } | prod ~ "/" ~ unaryOp ^^ {
          case e1~"/"~e2 => BinOp(DivOp, e1,e2)
        } | unaryOp

      lazy val lambda: P[Expr] =
        ("\\" ~> ident) ~ ("." ~> expression) ^^ {
        case arg~body => Lambda(arg, body)
      }

      lazy val ifExpr: P[Expr] =
      ("if" ~> expression) ~
        ("then" ~> expression) ~
        ("else" ~> expression) ^^ {
          case cond~e1~e2 => IfThenElse(cond,e1,e2)
        }

      lazy val letExpr: P[Expr] =
      ("let" ~> ident) ~ ("=" ~> expression) ~ ("in" ~> expression) ^^ {
        case binder~e1~e2 => LetIn(binder,e1,e2)
      }

      lazy val unaryOp: P[Expr] =
        { ("!"~>fact) ^^ NotOp } | fact

      lazy val application: P[Expr] = {
        (fact~"("~expression~")") ^^ {
          case e1~_~e2~_ => Apply(e1,e2)
        }
      }

      lazy val invocation: P[Expr] = {
        (fact~"."~ident) ^^ {
          case e1~_~lbl => Invoke(e1, lbl)
        }
      }

      lazy val operations: P[Expr] = (
        invocation |
        application
      )

      lazy val methodBody: P[Method] = {
        "("~ident~")"~"=>"~expression ^^ {
          case "("~selfBnd~")"~"=>"~body => Method(selfBnd, body)
        }
      }

      lazy val objectField: P[(Label, Method)] = {
        (ident~"="~"("~ident~")"~"=>"~expression) ^^ {
          case x~_~_~selfBnd~_~_~body => (x, Method(selfBnd, body))
        }
      }

      lazy val objectFields: P[List[(Label, Method)]] = {
        (objectField~";"~objectFields) ^^ {
          case f~";"~fs => f :: fs
        } | objectField ^^ { case f => List(f) }
      }

      lazy val obj: P[Expr] = {
         ("["~"]" ^^ { case _~_ => Object(ListMap()) }
         | ("[" ~> objectFields <~ "]") ^^ {
          case fieldList => Object(ListMap(fieldList: _*)) // Scala really does have some voodoo, doesn't it
        })
      }

      lazy val update: P[Expr] = {
        ("update"~>simpleExpr~"::"~ident~"with"~"("~methodBody~")") ^^ {
          case obj~_~lbl~_~_~body~_ => Update(obj, lbl, body)
        }
      }

      lazy val expression: P[Expr] =
        simpleExpr

      lazy val simpleExpr: P[Expr] = (
          obj |
          lambda |
          letExpr |
          ifExpr |
          binaryOp |
          fact
      )

      lazy val fact: P[Expr] = (
        update |
        operations |
        (ident ^^ Var) |
        (numericLit ^^ { x => NumV(x.toInt) }) |
        (stringLit ^^ { x => StrV(x) }) |
        ("true" ^^^ BoolV(true)) |
        ("false" ^^^ BoolV(false)) |
        "("~>expression<~")"
      )
    }
  }

  // Parsing of Source language
  object ParseSource {

    class SourceParser extends StandardTokenParsers with PackratParsers {
      import Assignment3.Source.Source._


      def parseStr(input: String): Expr = {
        phrase(expression)(new lexical.Scanner(input)) match {
          case Success(ast, _) => ast
          case e: NoSuccess => sys.error(e.toString)
        }
      }

      def parse(input: String): Expr = {
        val source = scala.io.Source.fromFile(input)
        val lines = try source.mkString finally source.close()
        parseStr(lines)
      }

      lexical.reserved += ("let", "in", "if", "then", "else", "true",
        "false", "type", "object", "class", "extends", "overrides", "new", "Class", "Object", "Top", "fun",
      "Int", "Bool", "String", "method", "root")
      lexical.delimiters += ("=", ".", "==",
        "=>", "(", ")", "+", "-", "*", "/", "&&", "||", "\\", ";", "<=", "!", "\"",
        "{", "}", "^", "[", "]", ":", "->", ":=", ",")

      type P[+A] = PackratParser[A]

      // Types
      lazy val objTyField: P[(Label, Type)] = {
        (ident~":"~ty) ^^ { case lbl~_~ty => (lbl, ty) }
      }

      lazy val objTyFields: P[List[(Label, Type)]] = {
        objTyField~(";"~>objTyFields) ^^ { case f~fs => f :: fs } |
        objTyField ^^ { case f => List(f) }
      }

      lazy val maybeObjTyFields: P[ListMap[Label, Type]] = {
        "["~"]" ^^ { case _ => ListMap(): ListMap[Label, Type] } |
        "["~>objTyFields<~"]" ^^ { case fields => ListMap(fields : _*) }
      }


      lazy val tyList: P[List[Type]] = {
        ty~(","~>tyList) ^^ { case typ~typs => typ :: typs } |
        ty ^^ { case typ => List(typ) }
      }


      lazy val fnTy : P[Type] = {
        ("("~>tyList<~")") ~ "->" ~ ty ^^ {
          case tys~"->"~resTy => TyFun(tys, resTy)
        } | objTy
      }

      lazy val objTy: P[Type] =
        "Object"~>("("~>ident<~")")~maybeObjTyFields ^^ {
        case selfVar~fields => TyObject(selfVar, fields)
      } | classTy

      lazy val classTy: P[Type] =
        "Class"~>"("~>ty<~")" ^^ {
          case classTy => TyClass(classTy)
        } | baseTy

      lazy val baseTy: P[Type] =
        ("Int" ^^^ TyInt) |
        ("Bool" ^^^ TyBool) |
        ("String" ^^^ TyString) |
        ("Top" ^^^ TyTop) |
        ident ^^ { case tv => TyVar(tv) } |
        "("~>ty<~")"

      lazy val ty: P[Type] =
        fnTy


      // Terms
      //
      lazy val binaryOp: P[Expr] = {
        binRel
      }

      lazy val methodUpdate: P[(TermVariable, Type, Expr)] = {
        "method"~"("~ident~":"~ty~")"~"{"~expression~"}" ^^ {
          case _~_~selfVar~_~selfTy~_~_~body~_ => (selfVar, selfTy, body)
        }
      }

      lazy val binRel: P[Expr] =
        expression ~ "==" ~ summation ^^ {
          case e1~_~e2 => BinOp(EqOp, e1,e2)
        } | expression ~ "&&" ~ summation ^^ {
          case e1~_~e2 => BinOp(AndOp, e1,e2)
        } | expression ~ "||" ~ summation ^^ {
          case e1~_~e2 => BinOp(OrOp, e1,e2)
        } | summation

      lazy val summation: P[Expr] =
        summation ~ "+" ~ prod ^^ {
          case e1~"+"~e2 => BinOp(AddOp, e1, e2)
        } | summation ~ "-" ~ prod ^^ {
          case e1~"-"~e2 => BinOp(SubOp, e1, e2)
        } | prod

      lazy val prod: P[Expr] =
        prod ~ "*" ~ unaryOp ^^ {
          case e1~"*"~e2 => BinOp(MulOp, e1,e2)
        } | prod ~ "/" ~ unaryOp ^^ {
          case e1~"/"~e2 => BinOp(DivOp, e1,e2)
        } | unaryOp


      lazy val unaryOp: P[Expr] =
        { ("!"~>fact) ^^ NotOp } | fact

      lazy val param: P[(TermVariable, Type)] = {
        ident~":"~ty ^^ { case param~_~ty => (param, ty) }
      }

      lazy val params: P[List[(TermVariable, Type)]] = {
        param~","~params ^^ { case param~_~params => param :: params } |
        param ^^ { case param => List(param) }
      }

      lazy val maybeParams: P[List[(TermVariable, Type)]] = {
        ("("~")" ^^ { case _ => List(): List[(TermVariable, Type)] }) |
        "("~>params<~")" ^^ { case parameters => parameters }
      }

      lazy val func: P[Expr] = {
        ("fun"~>maybeParams)~("{"~>expression<~"}") ^^ {
          case params~body => Func(params, body)
        }
      }

      lazy val ifExpr: P[Expr] =
      ("if" ~> expression) ~
        ("then" ~> expression) ~
        ("else" ~> expression) ^^ {
          case cond~e1~e2 => IfThenElse(cond,e1,e2)
        }

      lazy val letExpr: P[Expr] =
      ("let" ~> ident) ~ ("=" ~> expression) ~ ("in" ~> expression) ^^ {
        case binder~e1~e2 => LetIn(binder,e1,e2)
      }

      lazy val tyDef: P[Expr] = {
        ("type"~>ident)~("="~>ty)~("in"~>expression) ^^ {
          case tyname~ty~e => TypeIn(tyname, ty, e)
        }
      }

      lazy val objectField: P[(Label, Expr)] = {
        (ident~("="~>expression)) ^^ {
          case lbl~body => (lbl, body)
        }
      }

      lazy val objectFields: P[List[(Label, Expr)]] = {
        (objectField~";"~objectFields) ^^ {
          case f~";"~fs => f :: fs
        } | objectField ^^ { case f => List(f) }
      }

      lazy val maybeObjectFields: P[List[(Label, Expr)]] = {
        ("{"~"}" ^^ { case _ => List(): List[(Label, Expr)] }) |
         "{"~>objectFields<~"}" ^^ {
            case fields => fields
         }
      }

      lazy val obj: P[Expr] = {
        "object"~>"("~>(ident~":"~ty)~(")"~>maybeObjectFields) ^^ {
          case (selfBnd~_~ty)~fields => Object(selfBnd, ty, ListMap(fields: _*))
        }
      }

      lazy val simpleClass: P[Expr] = {
        "class"~>"("~>(ident~":"~ty)~(")"~>maybeObjectFields) ^^ {
          case (selfBnd~_~ty)~fields => Class(selfBnd, ty, None, ListMap(fields: _*), ListMap())
        }
      }

      lazy val extendsClass: P[Expr] = {
        "class"~"("~ident~":"~ty~")"~"extends"~"("~expression~":"~ty~")"~maybeObjectFields~"overrides"~maybeObjectFields ^^ {
          case _~_~selfBnd~_~selfTy~_~_~_~superExpr~_~superTy~_~fields~_~overrides => { // daaaang that's an ugly pattern
            val fieldsMap = ListMap(fields: _*)
            val overridesMap = ListMap(overrides: _*)
            Class(selfBnd, selfTy, Some((superExpr, superTy)), fieldsMap, overridesMap)
          }
        }
      }

      lazy val newClass: P[Expr] = {
        "new"~>expression ^^ { case e => New(e) }
      }

      lazy val clazz: P[Expr] = simpleClass | extendsClass



      lazy val appArgs: P[List[Expr]] = {
        fact~","~appArgs ^^ { case arg~_~args => arg :: args } |
        fact ^^ { case arg => List(arg) }
      }

      lazy val maybeAppArgs: P[List[Expr]] = {
        "("~>appArgs<~")" ^^ { case args => args } |
        "("~")" ^^ { case _ => List(): List[Expr] }
      }

      lazy val pathArgs: P[List[String]] = {
        ident~("."~>pathArgs) ^^ {
          case lbl~lbls => lbl :: lbls
        } |
        ident ^^ { case lbl => List(lbl) }
      }

      lazy val path: P[(Expr, List[String])] = {
        simpleExpr~("."~>pathArgs) ^^ {
          case e~args => (e, args)
        }
      }

      lazy val pathExpr: P[Expr] = {
        def pathToExprPair(baseObj: Expr, xs: List[String]): (Expr, String) = xs match {
          case Nil => sys.error("impossible case")
          case x::Nil => (baseObj, x)
          case x::xs => pathToExprPair(SelectField(baseObj, x), xs)
        }

        path~(":="~>methodUpdate) ^^ {
          case (e, xs)~mupd => {
            val (base, lbl) = pathToExprPair(e, xs)
            val (selfVar, selfTy, body) = mupd;
            MethodUpdate(base, lbl, selfVar, selfTy, body)
          }
        } | path~(":="~>simpleExpr) ^^ { case (e, xs)~assignE => {
            val (base, lbl) = pathToExprPair(e, xs)
            FieldUpdate(base, lbl, assignE)
          }
        } | path~maybeAppArgs ^^ { case (e, xs)~args => {
            val (base, lbl) = pathToExprPair(e, xs)
            Apply(SelectField(e, lbl), args)
          }
        } | path ^^ { case (e, xs) => {
              val (base, lbl) = pathToExprPair(e, xs)
              SelectField(base, lbl)
            }
          }
      }

      lazy val expression: P[Expr] =
          pathExpr | simpleExpr

      lazy val application: P[Expr] = {
        (fact~maybeAppArgs) ^^ {
          case e~args => Apply(e,args)
        }
      }

      lazy val simpleExpr: P[Expr] = (
          obj |
          clazz |
          newClass |
          func |
          tyDef |
          letExpr |
          ifExpr |
          binaryOp |
          fact
      )

      lazy val fact: P[Expr] = (
        application |
        ("root" ^^^ RootClass ) |
        (ident ^^ Var) |
        (numericLit ^^ { x => Num(x.toInt) }) |
        (stringLit ^^ { x => Str(x) }) |
        ("true" ^^^ Bool(true)) |
        ("false" ^^^ Bool(false)) |
        "("~>expression<~")"
      )
    }

  }

  def main( args:Array[String] ):Unit = {
    val FILENAME = "filename"
    val MODE = "mode"

    val CORE = "core"
    val SUGARED = "sugared"

    val defaultArgs = ListMap(
      FILENAME -> "",
      MODE -> "" // blehh
    )

    def readArgs(argList: List[String], optMap: ListMap[String, String]):
      ListMap[String, String] = argList match {
        case Nil => optMap
        case "-core" :: tail =>
          readArgs(tail, optMap + (MODE -> CORE))
        case "-source" :: tail =>
          readArgs(tail, optMap + (MODE -> SUGARED))
        case fn :: Nil => optMap + (FILENAME -> fn)
        case _ :: xs => readArgs(xs, optMap)
    }

    if (args.length == 0) {
      print("Usage: [-core | -source] filename\n")
    } else {

      val argMap = readArgs(args.toList, defaultArgs)

      val filename = argMap(FILENAME)
      if (argMap(MODE) == CORE) {
        import Assignment3.Core._;
        println("Parsing " + filename)
        val parser = new ParseCore.CoreParser
        val parsed = parser.parse(filename)
        println("Parsed: " + parsed.toString)
        val evaluated = Core.eval(parsed)
        println("Evaluated: " + evaluated.toString)
      } else if (argMap(MODE) == SUGARED) {
        import Assignment3.Source._;
        import Assignment3.SourceHelpers._;
        import Assignment3.Desugar._;
        import Assignment3.Core._;

        val parser = new ParseSource.SourceParser
        val expr = parser.parse(filename)
        println("Parsed: " + expr.toString)
        println("Resolving type aliases...")
        val resolvedExpr = SourceHelpers.resolveTypeAliases(expr)
        println("Typechecking...")
        val ty = Source.typecheckProgram(resolvedExpr)
        println("Type: " + ty.toString)
        println("Translating to core...")
        val coreProg = Desugar.desugar(resolvedExpr)
        println("Core program: " + coreProg.toString)
        println("Evaluating...")
        val evalRes = Core.eval(coreProg)
        println("Result: " + evalRes.toString)
      } else {
        println("Please specify either -core or -source!")
      }
    }
  }

}
// vim: set ts=2 sw=2 et sts=2:
