package mltt

import scala.scalajs.js
import scala.scalajs.js.annotation._

@js.native
@JSImport("fs", JSImport.Namespace)
object NodeFs extends js.Object {
  def readFileSync(path: String, encoding: String): String = js.native
}

class SubstMlttTests extends munit.FunSuite {
  import syntax._

  test("predef") {
    val predef = NodeFs.readFileSync("samples/predef.mltt", "utf-8")
    val prog = Parser.parse(predef)
    val env = prog.defs.foldLeft(Map.empty[String, Expr]) {
      case (e, Def(name, anno, init)) =>
        implicit val env = e
        val n = normalize(init)
        val inferred = infer(n)
        val Id = Lam("x", inferred, Var("x"))
        // Type Check
        anno.foreach { anno => normalize(App(Id, anno)) }
        e + (name -> inferred)
    }
  }
}
