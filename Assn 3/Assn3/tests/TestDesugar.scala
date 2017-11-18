package Assignment3Tests.TestDesugar
import org.scalatest._

import Assignment3.Assn3.Assn3._;
import Assignment3Tests.TestCommon._

import Assignment3.Desugar.Desugar._
import Assignment3.Core._
import Assignment3.Source._
import Assignment3.SourceHelpers._

object TestHelpers {
  def assertDesugared(sugaredStr: String, coreStr: String) = {
    val desugared = desugar(SourceHelpers.resolveTypeAliases(TestSourceHelpers.parseStr(sugaredStr)))
    val target = TestCoreHelpers.parseStr(coreStr)
    assert(
      TestCoreHelpers.aequiv(desugared, target),
    "Invalid desugaring. Expected " + target.toString + ", got: " + desugared.toString)
  }
}


import TestHelpers._

class DesugarSpec extends FlatSpec {

  "The desugaring function" should "correctly desugar objects" in {
    val objStr =
      "type Colour = Object(X)[red: Int; green: Int; Blue: Int; getRed: Int; addReds: (X) -> Int] in " +
      "object(this: Colour) { red = 255; green = 0; blue = 0; getRed = this.red; addReds = fun(other: Colour) { (this.red) + (other.red) } }"
    val targetStr =
      "[ red = (this) => 255; green = (this) => 0; blue = (this) => 0; " +
      "  getRed = (this) => this.red; addReds = (this) => \\other. (this.red) + (other.red) ]"
    assertDesugared(objStr, targetStr)
  }

  it should "correctly desugar multi-argument functions" in {
    assertDesugared("fun (x: Int, y: Bool, z: String) { y }",
      "\\x . \\y . \\z. y")
    // Because I have *so* much faith in my parser-writing skills...
    assert(
      TestCoreHelpers.aequiv(
        desugar(TestSourceHelpers.parseStr("fun (x: Int, y: Bool, z: String) { y }")),
        Core.Lambda("x", Core.Lambda("y", Core.Lambda("z", Core.Var("y"))))
      )
    )
  }

  it should "correctly desugar multi-argument function application" in {
    import Core._;

    val sugaredStr = "f(1,2,true)"
    val desugaredExpr = Apply(Apply(Apply(Var("f"), NumV(1)), NumV(2)), BoolV(true))
    assert(
      TestCoreHelpers.aequiv(
        desugar(TestSourceHelpers.parseStr(sugaredStr)),
        desugaredExpr))
  }

  it should "correctly desugar 'new'" in {
    assertDesugared("new pines", "pines.new")
  }

  it should "correctly desugar the root class" in {
    assertDesugared("root", "[ new = (z) => [] ]")
  }

  it should "correctly desugar method access" in {
    assertDesugared("x.y.z", "x.y.z")
  }

  it should "correctly desugar field updates" in {
    assertDesugared("x.l := 60", "update x::l with ((y) => 60)")
  }

  it should "correctly desugar method updates" in {
    assertDesugared("x.l := method(x: Int) { x.red }", "update x::l with ((y) => y.red)")
  }

  it should "correctly desugar let-bindings" in {
    assertDesugared("let x = 10 + 30 in 100 + 30",
      "let x = 10 + 30 in 100 + 30")
  }

  it should "correctly desugar numbers" in {
    assertDesugared("100", "100")
  }

  it should "correctly desugar binary operators" in {
    assertDesugared("100 * 110", "100 * 110")
    assertDesugared("100 + 110", "100 + 110")
    assertDesugared("100 / 110", "100 / 110")
    assertDesugared("100 - 110", "100 - 110")
    assertDesugared("(100 * 110) + 10", "(100 * 110) + 10")
    assertDesugared("true && (true || false)", "true && (true || false)")
  }

  it should "correctly desugar unary operators" in {
    assertDesugared("!true", "!true")
  }

  it should "correctly desugar if-then-else" in {
    assertDesugared("if (new e) then 100 + 20 else (new t)",
      "if e.new then 100 + 20 else t.new")
  }

  it should "correctly desugar booleans" in {
    assertDesugared("true", "true")
    assertDesugared("false", "false")
  }

  it should "correctly desugar plain classes" in {
    val sugaredStr =
      "type Ty = Object(X)[hello: String; world: String; sayHello: String] in " +
      "class (this: Ty) { hello = \"hello\"; world = \"world\"; sayHello = this.hello }"
    val desugaredStr =
      "[ new = (a) => [ hello = (b) => a.hello(b); world = (b) => a.world(b); sayHello = (b) => a.sayHello(b) ];" +
      "  hello = (c) => \\this. \"hello\"; " +
      "  world = (c) => \\this. \"world\"; " +
      "  sayHello = (c) => \\this. this.hello]"
    assertDesugared(sugaredStr, desugaredStr)
  }

  it should "correctly desugar classes with an inherited method" in {
    val sugaredStr =
      "type Ty1 = Object(X)[hello: String] in " +
      "type Ty2 = Object(X)[hello: String; world: String] in " +
      "let classTy1 = class(this: Ty1) { hello = \"hello\" } in " +
      "class(this: Ty2) extends(classTy1: Class(Ty2)) { world = \"world\" } overrides { }"

    val desugaredStr =
      "let classTy1 = [ new = (a) => [ hello = (b) => a.hello(b) ]; hello = (c) => \\this. \"hello\" ] in " +
      "[ new = (a) => [ hello = (b) => a.hello(b); world = (b) => a.world(b) ]; " +
      "  hello = (c) => classTy1.hello; " +
      "  world = (c) => \\this. \"world\" ]"

    assertDesugared(sugaredStr, desugaredStr)
  }

  it should "correctly desugar classes with an overridden method" in {
    val sugaredStr =
      "type Ty1 = Object(X)[hello: String] in " +
      "let classTy1 = class(this: Ty1) { hello = \"hello\" } in " +
      "class(this: Ty1) extends(classTy1: Class(Ty1)) {  } overrides { hello = \"hi\" }"

    val desugaredStr =
      "let classTy1 = [ new = (a) => [ hello = (b) => a.hello(b) ]; hello = (c) => \\this. \"hello\" ] in " +
      "[ new = (a) => [ hello = (b) => a.hello(b) ]; " +
      "  hello = (c) => \\this. \"hi\" ]"

    assertDesugared(sugaredStr, desugaredStr)
  }

  it should "correctly desugar classes with a declared method, an inherited method, and an overridden method" in {
    val sugaredStr =
      "type Ty1 = Object(X)[hello: String; world: String] in " +
      "type Ty2 = Object(X)[hello: String; world: String; frenchGreeting: String] in " +
      "let classTy1 = class(this: Ty1) { hello = \"hello\"; world = \"world\" } in " +
      "class(this: Ty2) extends(classTy1: Class(Ty2)) { frenchGreeting = \"bonjour\" } overrides { world = \"universe\" }"

    val desugaredStr =
      "let classTy1 = [ new = (a) => [ hello = (b) => a.hello(b); world = (b) => a.world(b) ]; " +
      "  hello = (c) => \\this. \"hello\"; " +
      "  world = (c) => \\this. \"world\" ] in " +
      "[ new = (a) => [ hello = (b) => a.hello(b); world = (b) => a.world(b); " +
      "    frenchGreeting = (b) => a.frenchGreeting(b) ]; " +
      "  hello = (c) => classTy1.hello; " +
      "  world = (c) => \\this. \"universe\"; " +
      "  frenchGreeting = (c) => \\this. \"bonjour\" ]"

    assertDesugared(sugaredStr, desugaredStr)
  }
}


// vim: set ts=2 sw=2 et sts=2:
