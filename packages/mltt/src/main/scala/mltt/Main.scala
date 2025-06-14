package mltt

import syntax._

type Env = Map[String, Expr]

def infer(expr: Expr)(implicit env: Env) = ???;
def typeEq(lhs: Type, rhs: Type) = ???;
def subst(n: String, into: Expr, expr: Expr): Expr = ???;
def normalize(expr: Expr)(implicit env: Env): Expr = ???;

type Value = scala.Nothing;
