package lyzh.syntax

import fastparse._, fastparse.ScalaWhitespace._

case class Let(
    name: String,
    params: Option[List[Param]],
    ty: Option[Expr],
    init: Expr,
)
case class Prog(defs: List[Let])

sealed abstract class Expr
final case class Name(name: String) extends Expr
final case class UniE(level: Option[Int]) extends Expr
final case class Lam(p: Param, body: Expr, lvl: Boolean) extends Expr
final case class App(func: Expr, arg: Expr) extends Expr
final case class Param(name: String, ty: Option[Expr])

type Type = Expr

object Parser {
  def root[$: P] = P(("" ~ stmt).rep.map(_.toList).map(Prog.apply) ~ End)

  def stmt[$: P]: P[Let] = P(valDef | defDef)
  def valDef[$: P] = letDef("val" | "var" | "type", "".map(_ => None))
  def defDef[$: P] = letDef("def", param.rep.map(_.toList).?)
  def letDef[$: P](k: => P[Unit], p: => P[Option[List[Param]]]): P[Let] =
    P(kw(k) ~/ id ~ p ~ (":" ~/ term).? ~ "=" ~/ term).map(Let.apply)

  def term[$: P]: P[Expr] = P(uniTerm | lamTerm | appTerm | name)
  def uniTerm[$: P] = P(kw("Type")).map(_ => UniE(None))
  def lamTerm[$: P] = P(lamParam ~ ("=>" | "->").! ~/ term).map {
    case (p, arrow, body) => Lam(p, body, arrow == "->")
  }
  def appTerm[$: P] =
    P(name ~ ("(" ~/ term.rep(sep = ",") ~ ",".? ~ ")").rep)
      .map { case (func, args) => args.flatten.foldLeft(func)(App.apply) }
  def name[$: P] = P(id).map(Name.apply)

  def param[$: P] = P("(" ~/ id ~ (":" ~/ term).? ~ ")").map(Param.apply)
  def lamParam[$: P] = param | id.map(name => Param(name, None))

  def kw[$: P](s: => P[Unit]) = s ~~ !idCont
  def letter[$: P] = P(CharIn("a-z") | CharIn("A-Z"))
  def digit[$: P] = P(CharIn("0-9"))
  def id[$: P] = P((letter | "_") ~~ idCont.repX).!.filter(!keywords(_))
  def idCont[$: P] = P(letter | digit | "_")

  val keywords = Set("type", "var", "val", "def")
}
