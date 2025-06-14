package mltt

import syntax._

type Env = Map[String, Expr]

object Subst {
  def infer(expr: Expr)(implicit env: Env): Type = expr match {
    case Var(name) =>
      env.getOrElse(name, { throw Error(s"unknown variable ${name}") })
    case Uni(lvl) => Uni(lvl + 1)
    case Pi(name, ty, retTy) =>
      Uni(typeLvl(ty).max(typeLvl(retTy)(env + (name -> ty))))
    case Lam(name, ty, body) =>
      val pty = normalize(ty)
      Pi(name, pty, infer(body)(env + (name -> pty)))
    case App(func, arg) =>
      val (funcTy, argTy) = (infer(func), infer(arg))
      normalize(funcTy) match {
        case Pi(name, paramTy, retTy) =>
          if !typeEq(paramTy, argTy) then
            throw Error(s"type mismatch, ${paramTy} v.s. ${argTy}")
          normalize(subst(name, arg, retTy))
        case ty => throw Error(s"expect function to call, got ${ty}")
      }
  }
  def isType(expr: Expr)(implicit env: Env): Type = { typeLvl(expr); expr }
  def typeLvl(expr: Expr)(implicit env: Env): Int = {
    normalize(infer(expr)) match {
      case Uni(level) => level
      case Pi(name, ty, body) =>
        val tyLvl = typeLvl(ty)(env + (name -> ty))
        val bodyLvl = typeLvl(body)(env + (name -> ty))
        tyLvl.max(bodyLvl)
      case Var(name) =>
        env.get(name) match {
          case Some(ty) => typeLvl(ty)
          case None     => throw Error(s"unknown variable ${name}")
        }
      case rest => throw Error(s"expect type, got ${rest}")
    }
  }
  def typeEq(lhs: Type, rhs: Type)(implicit env: Env) =
    (αEquiv(normalize(lhs), normalize(rhs))(Map(), env));
  def αEquiv(expr: Expr, other: Expr)(implicit
      names: Map[String, String],
      env: Env,
  ): Boolean = {
    (expr, other) match {
      case (Var(name1), Var(name2)) =>
        names.getOrElse(name1, name1).contains(name2)
      case (Uni(lvl1), Uni(lvl2)) => lvl1 == lvl2
      case (Pi(name1, ty1, body1), Pi(name2, ty2, body2)) =>
        αEquiv(ty1, ty2) && αEquiv(body1, body2)(names + (name1 -> name2), env)
      case (Lam(name1, ty1, body1), Lam(name2, ty2, body2)) =>
        αEquiv(ty1, ty2) && αEquiv(body1, body2)(names + (name1 -> name2), env)
      case (App(func1, arg1), App(func2, arg2)) =>
        αEquiv(func1, func2) && αEquiv(arg1, arg2)
      case _ => false
    }
  }
  def subst(n: String, into: Expr, expr: Expr): Expr = expr match {
    case Var(name) if name == n => into
    case Var(_)                 => expr
    case Uni(lvl)               => Uni(lvl)
    case Pi(name, ty, body) =>
      if name != n then Pi(name, subst(n, into, ty), subst(n, into, body))
      else
        val freshen = s"θ${countVars(into, expr) + 1}"
        val fresh = subst(freshen, Var(freshen), _)
        Pi(freshen, subst(n, into, fresh(ty)), subst(n, into, fresh(body)))
    case Lam(name, ty, body) =>
      if name != n then Pi(name, subst(n, into, ty), subst(n, into, body))
      else
        val freshen = s"θ${countVars(into, expr) + 1}"
        val fresh = subst(freshen, Var(freshen), _)
        Pi(freshen, subst(n, into, fresh(ty)), subst(n, into, fresh(body)))
    case App(func, arg) =>
      App(subst(n, into, func), subst(n, into, arg))
  }
  def countVars(into: Expr, expr: Expr): Int = expr match {
    case Var(_) => 1
    case Uni(_) => 0
    case Pi(name, ty, body) =>
      countVars(into, ty) + countVars(into, body)
    case Lam(name, ty, body) =>
      countVars(into, ty) + countVars(into, body)
    case App(func, arg) =>
      countVars(into, func) + countVars(into, arg)
  }
  def normalize(expr: Expr)(implicit env: Env): Expr = {
    expr match {
      case Var(name) => Var(name)
      case Uni(lvl)  => Uni(lvl)
      case Pi(name, ty, body) =>
        Pi(name, normalize(ty), normalize(body))
      case Lam(name, ty, body) =>
        Lam(name, normalize(ty), normalize(body))
      case App(func, arg) =>
        normalize(func) match {
          case Var(name) =>
            env.get(name) match {
              case Some(ty) => normalize(App(ty, arg))
              case None     => App(func, normalize(arg))
            }
          case Lam(name, _, retTy) =>
            normalize(subst(name, arg, retTy))
          case Pi(name, _, retTy) =>
            normalize(subst(name, arg, retTy))
          case func => App(func, normalize(arg))
        }
    }
  }
}

type Value = scala.Nothing;
