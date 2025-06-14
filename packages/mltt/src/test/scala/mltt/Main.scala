package mltt

import scala.scalajs.js
import scala.scalajs.js.annotation._

@js.native
@JSImport("fs", JSImport.Namespace)
object NodeFs extends js.Object {
  def readFileSync(path: String, encoding: String): String = js.native
}

object Utils {
  def debugln1(f: Any) = {}
  def debugln(f: Any) = {
    println(f)
  }
}

object SubstMltt {

  import syntax._

  def loadProg(path: String) = Parser.parse(NodeFs.readFileSync(path, "utf-8"))
  def tyck(progs: List[Prog]) = {
    import Subst._;

    progs.map(_.defs).flatten.foldLeft(Map[String, Type](), List[Expr]()) {
      case ((e, tys), Def(isType, name, anno, init)) =>
        implicit val env = e
        val n = normalize(init)
        val inferred = infer(n)
        val Id = Lam("x", inferred, Var("x"))
        // Type Check
        anno.foreach { anno => normalize(App(Id, anno)) }
        val ty = if isType then n else inferred
        (env + (name -> ty), tys :+ ty)
    }
  }
}

class SubstMlttTests extends munit.FunSuite {
  import SubstMltt._
  import mltt.Utils.{debugln1 as debugln}

  def testit(path: String) = debugln(
    tyck(Seq("samples/predef.mltt", path).map(loadProg).toList),
  )

  test("predef") { testit("samples/predef.mltt") }
  test("exts") { testit("samples/exts.mltt") }
}

class NBEMlttTests extends munit.FunSuite {
  import SubstMltt.{loadProg, tyck => tyckSubst}
  import syntax._
  import mltt.Utils.debugln

  def tyck(progs: List[Prog]) = {
    import NBE._;

    progs.map(_.defs).flatten.zip(tyckSubst(progs)._2).foldLeft(Env()) {
      case (e, (Def(isType, name, anno, init), sty)) =>
        implicit val env = e
        val n = normalize(init)
        val inferred = infer(init)
        val ty = if isType then n else inferred

        val tyE = valueToExpr(ty)
        // todo: pass tests
        // assert(
        //   Subst.αEquiv(tyE, sty)(Map()),
        //   s"Type mismatch for $name: expected $sty, got $tyE",
        // )
        e.addVar(name, ty)
    }
  }

  def testit(path: String) = debugln(
    tyck(Seq("samples/predef.mltt", path).map(loadProg).toList),
  )

  test("predef") { testit("samples/predef.mltt") }
  test("exts") { testit("samples/exts.mltt") }
}
