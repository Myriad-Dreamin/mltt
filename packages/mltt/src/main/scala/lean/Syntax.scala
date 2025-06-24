package lean.syntax

import fastparse._, fastparse.ScalaWhitespace._
import NodeParse._

private type Str = String
private type Pol = Option[List[Param]]
private type No = Option[Expr]

case class Prog(defs: List[Expr])

sealed abstract class Expr {
  var offset: Int = -1;
  var end: Int = -1;
}

// id, Type, . => R, . -> R, . (Rs), op L
final case class Name(name: String) extends Expr
final case class UniE(level: Option[Int]) extends Expr
final case class Lam(p: Expr, body: Expr, lvl: Boolean) extends Expr
final case class App(func: Expr, arg: List[Expr], ct: Boolean) extends Expr
final case class UnOp(op: Str, lhs: Expr) extends Expr

// case L, case L => R, L op R, id: R, L match { Rs }, { Ls }, ( Ls ), { ..(case L)s }
final case class Case(cond: Expr, body: No) extends Expr
final case class BinOp(op: Str, lhs: Expr, rhs: Expr) extends Expr
final case class Keyed(lhs: Name, rhs: Expr) extends Expr
final case class Match(lhs: Expr, rhs: Expr) extends Expr
final case class Block(stmts: List[Expr]) extends Expr
final case class Param(arg: List[Expr], tl: Boolean, ct: Boolean) extends Expr
final case class CaseBlock(stmts: List[Case]) extends Expr

// def name params: ty body
final case class Let(name: Name, ps: Pol, ty: No, body: Expr) extends Expr
// class name params  body
final case class Class(name: Name, ps: Pol, body: Expr) extends Expr

type Type = Expr

object NodeParse {
  implicit class Mapper[T <: Expr](n: => P[T])(implicit ctx: P[_]) {
    def m = {
      val l = ctx.index;
      val r = n.map(node => { node.offset = l; node.end = ctx.index; node });
      ctx.asInstanceOf[P[T]]
    }
  }
}

object Parser {
  def root[$: P] = P(("" ~ stmt).rep.map(_.toList).map(Prog.apply) ~ End)

  def stmt[$: P]: P[Expr] = P(valDef | defDef | clsDef | casDef | term)
  def valDef[$: P] = letDef("val" | "var" | "type", "".map(_ => None))
  def defDef[$: P] = letDef("def", params.?)
  def letDef[$: P](k: => P[Unit], p: => P[Option[List[Param]]]): P[Let] =
    P(kw(k) ~/ name ~ p ~ (":" ~/ term).? ~ body).map(Let.apply).m
  def clsDef[$: P] =
    P(kw("class") ~/ name ~ params.? ~ body).map(Class.apply).m
  def casDef[$: P] =
    P(kw("case") ~/ factor).map {
      case Lam(lhs, rhs, false) => Case(lhs, Some(rhs))
      case x                    => Case(x, None)
    }

  def term[$: P] =
    P(orE ~ P(kw("match") ~/ braces).rep)
      .map { case (lhs, rhs) => rhs.foldLeft(lhs)(Match.apply) }
  def binOp[$: P](op: => P[Unit], next: => P[Expr]): P[Expr] =
    P(next ~ (op.! ~/ next).rep).map { case (lhs, rhs) =>
      rhs.foldLeft(lhs) { case (lhs, (op, rhs)) => BinOp(op, lhs, rhs) }
    }.m
  def orE[$: P] = binOp(P("or"), andE)
  def andE[$: P] = binOp(P("and"), addSub)
  def addSub[$: P] = binOp(CharIn("+\\-"), divMul)
  def divMul[$: P] = binOp(CharIn("*/"), factor)
  def unary[$: P] = P(("!" | "-" | "+").! ~ factor).map(UnOp.apply.tupled).m
  def primaryExpr[$: P] = P(uni | keyedOrName | parens | braces).m
  def factor[$: P]: P[Expr] = P(unary | primaryExpr.flatMapX(factorR))
  def eBinR[$: P](e: Expr) = app(e) | lam(e)
  def factorR[$: P](e: Expr): P[Expr] =
    P(("" ~ eBinR(e)).m.flatMapX(factorR) | P("").map(_ => e))

  def app[$: P](lhs: Expr) = args((x, _, y) => App(lhs, x, y))
  def lam[$: P](lhs: Expr) = P(("=>" | "->").! ~/ term).map {
    case (arrow, body) => Lam(lhs, body, arrow == "->")
  }
  def uni[$: P] = P(kw("Type")).map(_ => UniE(None))
  def keyedOrName[$: P] = (name ~ (":" ~/ term).?).map {
    case (n, Some(ty)) => Keyed(n, ty)
    case (n, None)     => n
  }
  def name[$: P] = P(id).map(Name.apply).m

  def parens[$: P]: P[Expr] = args((l, tl, ct) => {
    if !ct && !tl && l.length == 1 && !l.head.isInstanceOf[Keyed] then l.head
    else Param(l, tl, ct)
  })
  def braces[$: P]: P[Expr] =
    P("{" ~/ stmt.rep.map(_.toList) ~ "}").map(body => {
      // check if all terms are cases
      var caseItems = List.empty[Case]
      var anyNotCase = false
      body.foreach {
        case c: Case => caseItems = caseItems :+ c
        case _       => anyNotCase = true
      }
      if caseItems.isEmpty then Block(body) else CaseBlock(caseItems)
    })
  def caseItem[$: P] = P(kw("case") ~/ factor).map {
    case Lam(lhs, rhs, false) => Case(lhs, Some(rhs))
    case x                    => Case(x, None)
  }

  def body[$: P] = ("=" ~/ term) | braces
  def params[$: P] = args(Param.apply).rep.map(_.toList)
  def args[T, $: P](c: (List[Expr], Boolean, Boolean) => T) =
    brak("[", "]").map(c(_, _, true)) | brak("(", ")").map(c(_, _, false))
  def brak[$: P](l: => P[Unit], r: P[Unit]) = P(
    l ~/ term.rep(sep = ",").map(_.toList) ~ ("," ~ &(r)).?.map(!_.isEmpty) ~ r,
  )

  def kw[$: P](s: => P[Unit]) = s ~~ !idCont
  def letter[$: P] = P(CharIn("a-z") | CharIn("A-Z"))
  def digit[$: P] = P(CharIn("0-9"))
  def id[$: P] = P((letter | "_") ~~ idCont.repX).!.filter(!keywords(_))
  def idCont[$: P] = P(letter | digit | "_")

  val keywords = "type|var|val|def|class|case|and|or|match".split('|').toSet
}
