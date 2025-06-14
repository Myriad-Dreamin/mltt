package mltt

import scala.scalajs.js
import scala.scalajs.js.annotation._

@js.native
@JSImport("fs", JSImport.Namespace)
object NodeFs extends js.Object {
  def readFileSync(path: String, encoding: String): String = js.native
}

object Utils {
  def debugln(f: Any) = {}
}

class SubstMlttTests extends munit.FunSuite {
  import syntax._
  import mltt.Utils.debugln

  def loadProg(path: String) = Parser.parse(NodeFs.readFileSync(path, "utf-8"))
  def tyck(progs: List[Prog]) = {
    import Subst._;

    progs.map(_.defs).flatten.foldLeft(Map[String, Type]()) {
      case (e, Def(isType, name, anno, init)) =>
        implicit val env = e
        val n = normalize(init)
        val inferred = infer(n)
        val Id = Lam("x", inferred, Var("x"))
        // Type Check
        anno.foreach { anno => normalize(App(Id, anno)) }
        if isType then {
          e + (name -> n)
        } else {
          e + (name -> inferred)
        }
    }
  }

  def testit(path: String) = debugln(
    tyck(Seq("samples/predef.mltt", path).map(loadProg).toList),
  )

  test("predef") { testit("samples/predef.mltt") }
  test("exts") { testit("samples/exts.mltt") }
}
