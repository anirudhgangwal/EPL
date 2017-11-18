package Assignment3Tests.TestCore
import org.scalatest._
import Assignment3.Core.Core._
import scala.collection.immutable.ListMap

import Assignment3Tests.TestCommon.TestCoreHelpers._

class CoreSubstSpec extends FlatSpec {

  "The core substitution function" should "substitute for a matching variable" in {
    assertEquiv(subst(Var("x"), NumV(5), "x"), NumV(5))
  }

  it should "not substitute for a non-matching variable" in {
    assertEquiv(subst(Var("y"), NumV(5), "x"), Var("y"))
  }

  it should "apply substitution under application" in {
    assertEquiv(
      subst(
        subst(
          Apply(Var("x"),Var("y")),
          Var("w"),
          "x"),
        Var("z"),
        "y"),
    Apply(Var("w"),Var("z")))
  }

  it should "apply substitution under if-then-else" in {
    assertEquiv(
      subst(
        IfThenElse(Var("x"),Var("x"),Var("x")),
        Var("z"), "x"),
    IfThenElse(Var("z"),Var("z"),Var("z")))
  }

  it should "apply substitution under method invocation" in {
    assertEquiv(
      subst(Invoke(Object(ListMap("lbl1" -> Method("x", Var("y")))), "lbl1"),
            Var("z"), "y"),
      Invoke(Object(ListMap("lbl1" -> Method("x", Var("z")))), "lbl1"))
  }

  it should "apply substitution under binary operations" in {
    assertEquiv(
        subst(
          BinOp(EqOp, Var("x"),Var("x")),
          Var("z"),
          "x"),
      BinOp(EqOp, Var("z"),Var("z")))
  }

  it should "apply substitution under unary operations" in {
    assertEquiv(subst(NotOp(Var("x")), Var("y"), "x"), NotOp(Var("y")))
  }

  it should "apply substitution under lambda" in {
      assertEquiv(
        subst(
          Lambda("y", Var("x")),
          Var("z"),
          "x"),
      Lambda("a",Var("z")))
  }

  it should "avoid variable-capture under lambda" in {
    assertEquiv(
      subst(
        Lambda("y",BinOp(AddOp, Var("x"),Var("y"))),
        BinOp(AddOp, Var("y"),Var("y")), "x"),
    Lambda("a",BinOp(AddOp, BinOp(AddOp, Var("y"),Var("y")),Var("a"))))
  }

  it should "only substitute for free variables in lambda bodies" in {
    assertEquiv(
      subst(
        Lambda("y",BinOp(AddOp, Var("x"),Var("y"))),
        NumV(5), "y"),
      Lambda("y", BinOp(AddOp, Var("x"), Var("y"))))
  }

  it should "only substitute for free variables in method bodies" in {
    val obj = parseStr("[ myInt = (x) => 10; add5 = (x) => x.myInt + 5]");
    assertEquiv(subst(obj, NumV(10), "x"), obj)
  }

  it should "avoid variable capture in method bodies" in {
    val obj1 = parseStr("[ myInt = (x) => 10; add5 = (x) => x.myInt + z]");
    val obj2 = parseStr("[ myInt = (x) => 10; add5 = (a) => a.myInt + x]");
    assertEquiv(subst(obj1, Var("x"), "z"), obj2)
  }

  it should "apply substitution under method updates" in {
    val obj = parseStr("[ myInt = (x) => 10; add5 = (x) => x.myInt + y]");
    val newMeth = Method("x", BinOp(AddOp, Invoke(Var("x"), "myInt"), Var("y")))
    val expr = Update(obj, "add5", newMeth)
    val substituted = subst(expr, NumV(10), "y")
    val target = parseStr("update ([ myInt = (x) => 10; add5 = (x) => x.myInt + 10 ])::add5 with ((a) => a.myInt + 10)")
    assertEquiv(substituted, target)
  }

  it should "avoid variable capture under method updates" in {
    val obj = parseStr("[ myInt = (x) => 10; add5 = (x) => x.myInt + 5]");
    val newMeth = Method("y", BinOp(AddOp, Invoke(Var("y"), "myInt"), Var("z")))
    val expr = Update(obj, "add5", newMeth)
    val substituted = subst(expr, Var("y"), "z")
    val target = parseStr("update ([ myInt = (x) => 10; add5 = (x) => x.myInt + 5 ])::add5 with ((a) => a.myInt + y)")
    assertEquiv(substituted, target)
  }

}

class CoreEvalSpec extends FlatSpec {

  "The core evaluation function" should "correctly evaluate addition" in {
    assertEquiv(eval(BinOp(AddOp, NumV(2), NumV(4))), NumV(6))
  }

  it should "correctly evaluate Subtraction" in {
    assertEquiv(eval(BinOp(SubOp, NumV(2), NumV(2))), NumV(0))
  }

  it should "correctly evaluate Multiplication" in {
    assertEquiv(eval(BinOp(MulOp, NumV(2), NumV(4))), NumV(8))
  }

  it should "correctly evaluate Division" in {
    assertEquiv(eval(BinOp(DivOp, NumV(4), NumV(2))), NumV(2))
  }

  it should "correctly evaluate Equal Int (True)" in {
    assertEquiv(eval(BinOp(EqOp, NumV(2),NumV(2))), BoolV(true))
  }
  it should "correctly evaluate Equal Int (False)" in {
    assertEquiv(eval(BinOp(EqOp, NumV(2),NumV(3))), BoolV(false))
  }

  it should "correctly evaluate Equal Bool (True)" in {
    assertEquiv(eval(BinOp(EqOp, BoolV(true),BoolV(true))), BoolV(true))
  }

  it should "correctly evaluate Equal Bool (False)" in {
    assertEquiv(eval(BinOp(EqOp, BoolV(true),BoolV(false))), BoolV(false))

  }

  it should "correctly evaluate And Bool (True)" in {
    assertEquiv(eval(BinOp(AndOp, BoolV(true),BoolV(true))), BoolV(true))
  }

  it should "correctly evaluate And Bool (False)" in {
    assertEquiv(eval(BinOp(AndOp, BoolV(true),BoolV(false))), BoolV(false))
  }

  it should "correctly evaluate Or Bool (True)" in {
    assertEquiv(eval(BinOp(OrOp, BoolV(true),BoolV(false))), BoolV(true))
  }

  it should "correctly evaluate Or Bool (False)" in {
    assertEquiv(eval(BinOp(OrOp, BoolV(false),BoolV(false))), BoolV(false))
  }

  it should "correctly evaluate the 'true' branch of an 'if' statement" in {
    assertEquiv(
      eval(IfThenElse(BoolV(true),
           NumV(1),
           NumV(2))),
      NumV(1))
  }

  it should "correctly evaluate the 'false' branch of an 'if' statement" in {
    assertEquiv(
      eval(IfThenElse(BoolV(false),
           NumV(1),
           NumV(2))),
      NumV(2))
  }

  it should "correctly substitute under a let-binding" in {
    assertEquiv(
      eval(LetIn("x",NumV(0),Var("x"))),
      NumV(0)
    )
  }

  it should "correctly handle shadowing under let-binders" in {
      assertEquiv(
        eval(LetIn("x", NumV(0), LetIn("x", NumV(1), Var("x")))),
        NumV(1)
      )
  }

  it should "correctly handle function application" in {
    assertEquiv(
        eval(Apply(Lambda("x", Var("x")),NumV(1))),
        NumV(1)
    )
  }

  it should "correctly handle shadowing in function application" in {
    val test = Apply(Lambda("x", Apply(Lambda("x", Var("x")), NumV(6))), NumV(5))
    val res = eval(test)
    assertEquiv(res, NumV(6))
  }

  it should "correctly handle field accesses" in {
    val objStr = "([ oneHundred = (x) => 100; field2 = (self) => x.oneHundred ]).oneHundred"
    val obj = parseStr(objStr)
    assertEquiv(eval(obj), NumV(100))
  }

  it should "correctly handle self-variables" in {
    val objStr = "([ oneHundred = (x) => 100; field2 = (self) => self.oneHundred ]).field2"
    val obj = parseStr(objStr)
    assertEquiv(eval(obj), NumV(100))
  }

  it should "correctly calculate fac(10) = 3628800" in {
    val facStr = "([ fac = (self) => \\y. if (y == 0) then 1 else y * (self.fac(y - 1))]).fac(10)"
    val fac = parseStr(facStr)
    assertEquiv(eval(fac), NumV(3628800))
  }

  it should "correctly handle nested field accesses" in {
    val str =
      "let o2 = [ x = (y) => true ] in " +
      "let o1 = [ get = (z) => o2 ] in " +
      "o1.get.x"
    val parsed = parseStr(str)
    assertEquiv(eval(parsed), BoolV(true))
  }

  it should "correctly handle method updates" in {
    val str =
      "let o = [ numField = (x) => 100; addN = (x) => x.numField + 10 ] in " +
      "let o2 = update o::addN with ((y) => y.numField + 1000) in " +
      "o2.addN"
    val parsed = parseStr(str)
    assertEquiv(eval(parsed), NumV(1100))
  }

}

// vim: set ts=2 sw=2 et sts=2:
