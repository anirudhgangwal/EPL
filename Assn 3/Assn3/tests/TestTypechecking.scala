package Assignment3Tests.TestTypechecking
import org.scalatest._
import Assignment3.Source.Source._
import Assignment3.SourceHelpers._
import scala.collection.immutable.ListMap
import Assignment3Tests.TestCommon.TestSourceHelpers._
import Assignment3.Utility.Utility._

class TypecheckingSpec extends FlatSpec {
  def assertType(env: ListMap[Variable, Type], expr: Expr, ty: Type) = {
    val checkedTy = typeCheck(env, expr)
    assert(SourceHelpers.equivTypes(checkedTy, ty),
      "Typechecking failure -- expected " + ty.toString + " but got " + checkedTy.toString)
  }

  def at(expr: Expr, ty: Type) = {
    assertType(new ListMap(), expr, ty)
  }

  def assertError(env: ListMap[Variable, Type], expr: Expr) = {
    intercept[Exception] {
      typeCheck(env, expr)
    }
  }

  def ae(e: Expr) = {
    assertError(new ListMap(), e)
  }

  "The typechecker" should "correctly type a variable in the environment" in {
    assertType(ListMap("x" -> TyInt), Var("x"), TyInt)
  }

  it should "reject a variable not contained in the environment" in {
    assertError(ListMap("x" -> TyInt), Var("y"))
  }

  it should "correctly type a multi-argument function with only base types" in {
    val funStr = "fun(x: Int, y: Bool, z: String) { y }"
    val expr = parseStr(funStr)
    at(expr, TyFun(List(TyInt, TyBool, TyString), TyBool))
  }

  it should "correctly type a multi-argument function containing an object type" in {
    val funStr = "fun(x: Int, y: Object(X)[hello: String], z: String) { y }"
    val expr = parseStr(funStr)
    val objTy = TyObject("X", ListMap("hello" -> TyString))
    at(expr, TyFun(List(TyInt, objTy, TyString), objTy))
  }

  it should "correctly type a multi-argument application, with only base types" in {
    val env = ListMap("f" -> TyFun(List(TyInt, TyBool), TyString))
    val expr = parseStr("f(5, true)")
    assertType(env, expr, TyString)
  }

  it should "correctly type a multi-argument application, with no subtyping" in {
    val objTy = TyObject("X", ListMap("hello" -> TyString))
    val env = ListMap("f" -> TyFun(List(TyInt, objTy), TyString), "obj" -> objTy )
    val str ="f(5, obj)"
    val expr = parseStr(str)
    assertType(env, expr, TyString)

  }

  it should "correctly type a multi-argument application, with correct subtyping" in {
    val objFnTy = TyObject("X", ListMap("hello" -> TyString))
    val objTy = TyObject("X", ListMap("hello" -> TyString, "world" -> TyString))
    val env = ListMap("f" -> TyFun(List(TyInt, objFnTy), TyString), "obj" -> objTy)
    val str = "f(5, obj)"
    val expr = parseStr(str)
    assertType(env, expr, TyString)
  }

  it should "reject a multi-argument application with incorrect arguments" in {
    val env = ListMap("f" -> TyFun(List(TyInt, TyInt), TyString))
    val expr = parseStr("f(5, true)")
    assertError(env, expr)
  }

  it should "reject a multi-argument application with incorrect subtyping" in {
    val objFnTy = TyObject("X", ListMap("hello" -> TyString, "world" -> TyString))
    val objTy = TyObject("X", ListMap("hello" -> TyString))
    val env = ListMap("f" -> TyFun(List(TyInt, objFnTy), TyString), "obj" -> objTy)
    val str = "f(5, obj)"
    val expr = parseStr(str)
    assertError(env, expr)
  }

  it should "correctly type a let-bound argument" in {
    val str = "let x = 5 in x"
    at(parseStr(str), TyInt)
  }

  it should "correctly type integer literals" in {
    at(Num(10), TyInt)
  }

  it should "correctly type string literals" in {
    at(Str("Hello!"), TyString)
  }

  it should "correctly type boolean literals" in {
    at(Bool(false), TyBool)
  }

  it should "correctly type a well-typed simple arithmetic expression" in {
    val str = "1 + 3"
    at(parseStr(str), TyInt)
  }

  it should "correctly type a well-typed nested arithmetic expression" in {
    val str = "(1 + 2) * 3"
    at(parseStr(str), TyInt)
  }

  it should "rule out an ill-typed arithmetic expression" in {
    val str = "(1 + 2) * false"
    ae(parseStr(str))
  }

  it should "correctly type a well-typed boolean expression" in {
    val str = "true || false"
    at(parseStr(str), TyBool)
  }

  it should "correctly type a well-typed nested boolean expression" in {
    val str = "(true && true) || false"
    at(parseStr(str), TyBool)
  }

  it should "reject an ill-typed boolean expression" in {
    val str = "(true && 10) || false"
    ae(parseStr(str))
    val str2 = "(true && false) || 10"
    ae(parseStr(str2))
  }

  it should "correctly type a well-typed equality, where the arguments have the same base types" in {
    val tests =
      List(
        (ListMap("x1" -> TyBool, "x2" -> TyBool), "x1 == x2"),
        (ListMap("x1" -> TyString, "x2" -> TyString), "x1 == x2"),
        (ListMap("x1" -> TyInt, "x2" -> TyInt), "x1 == x2")
      )

    tests.foreach((entry: (ListMap[String, Type], String)) => {
      val (env, expStr) = entry;
      val expr = parseStr(expStr)
      val _ = assertType(env, expr, TyBool)
      ()
    })
  }

  it should "reject an equality where the arguments have differently-typed base types" in {
    val env = ListMap("e1" -> TyBool, "e2" -> TyInt)
    assertError(env, parseStr("e1 == e2"))
  }

  it should "reject an equality where the arguments have the same types, but not base types" in {
    val objTy = TyObject("X", ListMap("hello" -> TyString))
    val env = ListMap("e1" -> objTy, "e2" -> objTy)
    assertError(env, parseStr("e1 == e2"))
  }

  it should "accept a well-typed if-then-else expression" in {
    val str = "if (true || false) then 5 else 6"
    at(parseStr(str), TyInt)
  }

  it should "reject an if-then-else expression with a non-boolean predicate" in {
    val str = "if 5 then 10 else 12"
    ae(parseStr(str))
  }

  it should "reject an if-then-else expression with different branch types" in {
    val str = "if 5 then 10 else true"
    ae(parseStr(str))
  }

  it should "accept a well-typed object" in {
    val objTy = TyObject("Y", ListMap("hello" -> TyString, "number" -> TyInt))
    val objStr =
      "type ObjTy = Object(X)[hello: String; number: Int] in " +
      "object(this: ObjTy) { hello = \"hi\"; number = 5 }"
    at(parseStr(objStr), objTy)
  }

  it should "accept a well-typed object which uses a self-reference" in {
    val objTy = TyObject("Y", ListMap("hello" -> TyString, "hi" -> TyString))
    val objStr =
      "type ObjTy = Object(X)[hello: String; hi: String] in " +
      "object(this: ObjTy) { hello = \"hi\"; hi = (this.hello) }"
    at(parseStr(objStr), objTy)
  }

  it should "accept a well-typed object which uses a self-referencing type" in {
    val objTy = TyObject("Y", ListMap("hello" -> TyString, "hi" -> TyFun(List(TyVar("Y")), TyString)))
    val objStr =
      "type ObjTy = Object(X)[hello: String; hi: (X) -> String] in " +
      "object(this: ObjTy) { hello = \"hi\"; hi = fun(x: ObjTy) { (x.hello) }   }"
    at(parseStr(objStr), objTy)
  }

  it should "reject an object where not all fields are defined" in {
    val objStr =
      "type ObjTy = Object(X)[hello: String; hi: String] in " +
      "object(this: ObjTy) { hello = \"hi\" }"
    ae(parseStr(objStr))
  }

  it should "reject an object which has more fields than its type" in {
    val objStr =
      "type ObjTy = Object(X)[hello: String] in " +
      "object(this: ObjTy) { hello = \"hi\"; hi = \"hola\" }"
    ae(parseStr(objStr))
  }

  it should "reject an object where a field has the wrong type" in {
    val objStr =
      "type ObjTy = Object(X)[hello: String; number: Int] in " +
      "object(this: ObjTy) { hello = \"hi\"; number = true }"
    ae(parseStr(objStr))
  }

  it should "accept a well-typed 'new' expression" in {
    val objTy = TyObject("X", ListMap("hello" -> TyString))
    val env = ListMap("x" -> TyClass(objTy))
    assertType(env, New(Var("x")), objTy)
  }

  it should "reject a 'new' expression where the argument is not a class" in {
    val env = ListMap("x" -> TyInt)
    assertError(env, New(Var("x")))
  }

  it should "correctly type the 'root' class" in {
    at(RootClass, TyClass(TyObject("Z", ListMap())))
  }

  it should "accept a correctly-typed field selection" in {
    val objTy = TyObject("X", ListMap("hello" -> TyString))
    val env = ListMap("x" -> objTy)
    assertType(env, SelectField(Var("x"), "hello"), TyString)
  }

  it should "reject a field selection where the field label does not exist" in {
    val objTy = TyObject("X", ListMap("hello" -> TyString))
    val env = ListMap("x" -> objTy)
    assertError(env, SelectField(Var("x"), "hi"))
  }

  it should "accept a correctly-typed field update" in {
    val objTy = TyObject("X", ListMap("hello" -> TyString))
    val env = ListMap("x" -> objTy)
    val str = "x.hello := \"hi\""
    assertType(env, parseStr(str), objTy)
  }

  it should "reject a field update where the object does not have an object type" in {
    val env = ListMap("x" -> TyInt)
    val str = "x.hello := \"hi\""
    assertError(env, parseStr(str))
  }

  it should "reject a field update where the object type does not contain the given label" in {
    val objTy = TyObject("X", ListMap("hello" -> TyString))
    val env = ListMap("x" -> objTy)
    val str = "x.wassup := \"hi\""
    assertError(env, parseStr(str))
  }

  it should "reject a field update where the object type contains a label of a different type" in {
    val objTy = TyObject("X", ListMap("hello" -> TyString))
    val env = ListMap("x" -> objTy)
    val str = "x.hello := 180"
    assertError(env, parseStr(str))
  }

  it should "accept a correctly-typed method update" in {
    val objTy = TyObject("Y", ListMap("hello" -> TyString, "hi" -> TyString))
    val objStr =
      "type ObjTy = Object(X)[hello: String; hi: String] in " +
      "x.hi := method(thisAsWell: ObjTy) { thisAsWell.hello }"
    val env = ListMap("x" -> objTy)
    assertType(env, parseStr(objStr), objTy)
  }

  it should "reject a method update where the object does not have an object type" in {
    val objStr =
      "type ObjTy = Object(X)[hello: String; hi: String] in " +
      "x.hi := method(thisAsWell: ObjTy) { thisAsWell.hello }"
    val env = ListMap("x" -> TyString)
    assertError(env, parseStr(objStr))
  }

  it should "reject a method update where the object type does not contain the given label" in {
    val objTy = TyObject("Y", ListMap("hello" -> TyString, "hi" -> TyString))
    val objStr =
      "type ObjTy = Object(X)[hello: String; hi: String] in " +
      "x.wassup := method(this: ObjTy) { this.hello }"
    val env = ListMap("x" -> objTy)
    assertError(env, parseStr(objStr))
  }

  it should "reject a method update where the object type contains a label of a different type" in {
    val objTy = TyObject("Y", ListMap("hello" -> TyString, "hi" -> TyString))
    val objStr =
      "type ObjTy = Object(X)[hello: String; hi: String] in " +
      "x.hi := method(this: ObjTy) { 500 }"
    val env = ListMap("x" -> objTy)
    assertError(env, parseStr(objStr))
  }

  it should "reject a method which uses a self-variable of a different type" in {
    val objTy = TyObject("Y", ListMap("hello" -> TyString, "hi" -> TyString))
    val objStr =
      "type ObjTy = Object(X)[hello: String; hi: String] in " +
      "x.hi := method(fakeThis: String) { fakeThis }"
    val env = ListMap("x" -> objTy)
    assertError(env, parseStr(objStr))
  }


  it should "accept a well-typed plain class" in {
    val targetTy = TyClass(TyObject("X", ListMap("red" -> TyInt)))
    val classStr =
      "type ObjTy = Object(X)[red: Int] in " +
      "class (this: ObjTy) { red = 100 }"
    at(parseStr(classStr), targetTy)
  }

  it should "accept a well-typed extends class with a declared and an inherited method" in {
    val ty1 = TyObject("X", ListMap("red" -> TyInt))
    val ty2 = TyObject("X", ListMap("red" -> TyInt, "green" -> TyInt))
    val classStr =
      "type Ty1 = Object(X)[red: Int] in " +
      "type Ty2 = Object(X)[red: Int; green: Int] in " +
      "class(this: Ty2) extends(superclass: Class(Ty1)) { " +
      "  green = 100 " +
      "} overrides {} "
    val env = ListMap("superclass" -> TyClass(ty1))
    assertType(env, parseStr(classStr), TyClass(ty2))
  }

  it should "accept a well-typed extends class with a declared method, an inherited method, and an override method" in {
    val ty1 = TyObject("X", ListMap("red" -> TyInt, "green" -> TyInt))
    val ty2 = TyObject("X", ListMap("red" -> TyInt, "green" -> TyInt, "blue" -> TyInt))
    val classStr =
      "type Ty1 = Object(X)[red: Int; green: Int] in " +
      "type Ty2 = Object(X)[red: Int; green: Int; blue: Int] in " +
      "class(this: Ty2) extends(superclass: Class(Ty1)) { " +
      "  blue = 100 " +
      "} overrides { " +
      "  green = 9001 " +
      "} "
    val env = ListMap("superclass" -> TyClass(ty1))
    assertType(env, parseStr(classStr), TyClass(ty2))
  }

  it should "reject an extends class where superclass method types are not equivalent to inherited field types" in {
    val pt = TyObject("X", ListMap("x" -> TyInt))
    val superObjTy = TyObject("Y", ListMap("point" -> pt))
    val classStr =
      "type RednessPt = Object(X)[x: Int; redness: Int] in " +
      "type Pt = Object(X)[x: Int] in " +
      "type SuperObjTy = Object(X)[point: Pt] in " +
      "type ObjTy = Object(X)[point: RednessPt] in " +
      "class(this: ObjTy) extends(superclass: Class(SuperObjTy)) {} overrides {}"
    val env = ListMap("superclass" -> TyClass(superObjTy))
    assertError(env, parseStr(classStr))
  }

  it should "reject an extends class which does not extend another class" in {
    val classStr =
      "type ObjTy = Object(X)[red: Int] in " +
      "class (this: ObjTy) extends(x: Int) { red = 100 } overrides {}"
    ae(parseStr(classStr))
  }

  it should "reject an extends class which tries to inherit a field absent in the superclass" in {
    val ty1 = TyObject("X", ListMap("red" -> TyInt))
    val classStr =
      "type Ty1 = Object(X)[red: Int] in " +
      "type Ty2 = Object(X)[red: Int; green: Int] in " +
      "class(this: Ty2) extends(superclass: Class(Ty1)) { " +
      "  " +
      "} overrides {} "
    val env = ListMap("superclass" -> TyClass(ty1))

    assertError(env, parseStr(classStr))
  }

  it should "reject an extends class which contains names from a superclass, but not in overrides block" in {
    val ty1 = TyObject("X", ListMap("red" -> TyInt))
    val classStr =
      "type Ty1 = Object(X)[red: Int] in " +
      "type Ty2 = Object(X)[red: Int; green: Int] in " +
      "class(this: Ty2) extends(superclass: Class(Ty1)) { " +
      "  red = 100; " +
      "  green = 100 " +
      "} overrides {} "
    val env = ListMap("superclass" -> TyClass(ty1))
    assertError(env, parseStr(classStr))
  }

  it should "reject an extends class where the class type is not a subtype of the superclass type (width)" in {
    val superObjTy = TyObject("X", ListMap("red" -> TyInt, "blue" -> TyInt))
    val classStr =
      "type Ty1 = Object(X)[red: Int; blue: Int] in " +
      "type Ty2 = Object(X)[red: Int] in " +
      "class(this: Ty2) extends(superclass: Class(Ty1)) { " +
      "  red = 100 " +
      "} overrides { "+
      "  blue = 9000 " +
      "} "
    val env = ListMap("superclass" -> TyClass(superObjTy))
    assertError(env, parseStr(classStr))
  }

  it should "reject an extends class where the class type is not a subtype of the superclass type (depth)" in {
    val rednessPt = TyObject("X", ListMap("x" -> TyInt, "redness" -> TyInt))
    val superObjTy = TyObject("Y", ListMap("point" -> rednessPt))
    val classStr =
      "type RednessPt = Object(X)[x: Int; redness: Int] in " +
      "type Pt = Object(X)[x: Int] in " +
      "type SuperObjTy = Object(X)[point: RednessPt] in " +
      "type ObjTy = Object(X)[point: Pt] in " +
      "class(this: ObjTy) extends(superclass: Class(SuperObjTy)) {} overrides {}"
    val env = ListMap("superclass" -> TyClass(superObjTy))
    assertError(env, parseStr(classStr))
  }

  it should "reject an extends class where a declared method is not contained in the object type" in {
    val ty1 = TyObject("X", ListMap("red" -> TyInt))
    val classStr =
      "type Ty1 = Object(X)[red: Int] in " +
      "class(this: Ty1) extends(superclass: Class(Ty1)) { " +
      "  whatsredandhurtsyourteeth = \"a brick!\"" +
      "} overrides { " +
      "  red = 100 " +
      "} "
    val env = ListMap("superclass" -> TyClass(ty1))
    assertError(env, parseStr(classStr))
  }

  it should "reject an extends class where a declared method type does not match the object type" in {
    val ty1 = TyObject("X", ListMap("red" -> TyInt))
    val classStr =
      "type Ty1 = Object(X)[red: Int] in " +
      "type Ty2 = Object(X)[red: Int; green: Int] in " +
      "class(this: Ty2) extends(superclass: Class(Ty1)) { " +
      "  green = \"gruen\"" +
      "} overrides { " +
      "  red = 100 " +
      "} "
    val env = ListMap("superclass" -> TyClass(ty1))
    assertError(env, parseStr(classStr))
  }

  it should "reject an extends class where an override method typed does not match the object type" in {
    val ty1 = TyObject("X", ListMap("red" -> TyInt, "green" -> TyInt))
    val classStr =
      "type Ty1 = Object(X)[red: Int; green: Int] in " +
      "type Ty2 = Object(X)[red: Int; green: Int] in " +
      "class(this: Ty2) extends(superclass: Class(Ty1)) { " +
      "} overrides { " +
      "  red = 100; " +
      "  green = \"green\"" +
      "} "
    val env = ListMap("superclass" -> TyClass(ty1))
    assertError(env, parseStr(classStr))
  }

}

// vim: set ts=2 sw=2 et sts=2:
