package mltt.syntax

import fastparse._, fastparse.ScalaWhitespace._

case class Def(isType: Boolean, name: String, ty: Option[Expr], init: Expr)
case class Prog(defs: List[Def])

sealed abstract class Expr
final case class Var(name: String) extends Expr
final case class Uni(level: Int) extends Expr
final case class Pi(name: String, ty: Expr, body: Expr) extends Expr
final case class Lam(name: String, ty: Expr, body: Expr) extends Expr
final case class App(func: Expr, arg: Expr) extends Expr

type Type = Expr

object Parser {
  def parse(src: String): Prog = {
    fastparse.parse(src, Parser.root(_)) match {
      case Parsed.Success(prog, _) => prog
      case Parsed.Failure(_, index, extra) =>
        println(extra.trace().longAggregateMsg)
        println(src.slice(index, index + 40))
        throw new Exception("Parsing failed")
    }
  }

  def liftTy(expr: Expr): Expr = expr match {
    case Pi(name, ty, body)  => Pi(name, liftTy(ty), liftTy(body))
    case Lam(name, ty, body) => Pi(name, liftTy(ty), liftTy(body))
    case App(func, arg)      => App(liftTy(func), liftTy(arg))
    case other               => other
  }

  def root[$: P] = P(("" ~ stmt).rep.map(_.toList).map(Prog.apply) ~ End)

  def stmt[$: P]: P[Def] = P(type0Def | typeDef | valDef)
  def type0Def[$: P] = P(kw("type") ~ id ~ ";").map(Def(true, _, None, Uni(0)))
  def typeDef[$: P] =
    P(kw("type") ~/ id ~ ("=" ~/ term.map(liftTy)) ~ ";")
      .map(Def(true, _, None, _))
  def valDef[$: P] =
    P(kw("val") ~/ id ~ (":" ~/ term.map(liftTy)).? ~ "=" ~/ term ~ ";")
      .map(Def(false, _, _, _))

  def term[$: P]: P[Expr] = P(uniTerm | lamTerm | appTerm | varTerm)
  def uniTerm[$: P] = P(kw("Type")).map(_ => Uni(0))
  def lamTerm[$: P] =
    P("(" ~/ id ~ ":" ~ term.map(liftTy) ~ ")" ~ "=>" ~ term).map(Lam.apply)
  def appTerm[$: P] =
    P(varTerm ~ ("(" ~/ term.rep(sep = ",") ~ ",".? ~ ")").rep)
      .map { case (func, args) => args.flatten.foldLeft(func)(App.apply) }
  def varTerm[$: P] = P(id).map(Var.apply)

  def kw[$: P](s: String) = s ~~ !idCont
  def letter[$: P] = P(CharIn("a-z") | CharIn("A-Z"))
  def digit[$: P] = P(CharIn("0-9"))
  def id[$: P] = P((letter | "_") ~~ idCont.repX).!.filter(!keywords(_))
  def idCont[$: P] = P(letter | digit | "_")

  val keywords = Set("type", "var")
}
