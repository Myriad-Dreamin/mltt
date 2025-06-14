package mltt

import syntax._

object Subst {
  type Env = Map[String, Expr]

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
    (αEquiv(normalize(lhs), normalize(rhs))(Map()));
  def αEquiv(expr: Expr, other: Expr)(implicit
      names: Map[String, String],
  ): Boolean = {
    (expr, other) match {
      case (Var(name1), Var(name2)) =>
        names.getOrElse(name1, name1).contains(name2)
      case (Uni(lvl1), Uni(lvl2)) => lvl1 == lvl2
      case (Pi(name1, ty1, body1), Pi(name2, ty2, body2)) =>
        αEquiv(ty1, ty2) && αEquiv(body1, body2)(names + (name1 -> name2))
      case (Lam(name1, ty1, body1), Lam(name2, ty2, body2)) =>
        αEquiv(ty1, ty2) && αEquiv(body1, body2)(names + (name1 -> name2))
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
        val freshen = s"a'${countVars(into, expr) + 1}"
        val fresh = subst(freshen, Var(freshen), _)
        Pi(freshen, subst(n, into, fresh(ty)), subst(n, into, fresh(body)))
    case Lam(name, ty, body) =>
      if name != n then Pi(name, subst(n, into, ty), subst(n, into, body))
      else
        val freshen = s"a'${countVars(into, expr) + 1}"
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

object NBE {
  type VEnv = Map[String, Value]
  case class Env(
      types: VEnv = Map(),
      values: VEnv = Map(),
  ) {
    def addVal(name: String, value: Value): Env = {
      this.copy(values = values + (name -> value))
    }
    def addVar(name: String, ty: Value): Env = {
      Env(types + (name -> ty), values + (name -> NeuV(VarN(name))))
    }
  }

  sealed abstract class Neutral;
  case class VarN(name: String) extends Neutral;
  case class AppN(func: Neutral, arg: Value) extends Neutral;
  sealed abstract class Value;
  case class NeuV(neutral: Neutral) extends Value;
  case class UniV(level: Int) extends Value;
  case class PiV(ty: Value, impl: Value => Value) extends Value;
  case class LamV(ty: Value, impl: Value => Value) extends Value;

  def eval(expr: Expr)(implicit env: Env): Value = expr match {
    case Var(name) =>
      env.values.getOrElse(
        name,
        { throw Error(s"unknown variable ${name}") },
      )
    case Uni(lvl) => UniV(lvl)
    case Pi(name, ty, body) =>
      PiV(eval(ty), (arg: Value) => eval(body)(env.addVal(name, arg)))
    case Lam(name, ty, body) =>
      LamV(eval(ty), (arg: Value) => eval(body)(env.addVal(name, arg)))
    case App(func, arg) =>
      eval(func) match {
        case LamV(_, impl) => impl(eval(arg))
        case PiV(_, impl)  => impl(eval(arg))
        case NeuV(VarN(name)) =>
          env.types.getOrElse(
            name,
            { throw Error(s"unknown variable ${name}") },
          ) match {
            case LamV(_, impl) => impl(eval(arg))
            case PiV(_, impl)  => impl(eval(arg))
            case NeuV(neutral) => NeuV(AppN(neutral, eval(arg)))
            case _ => throw Error(s"expected function to apply, got ${func}")
          }
        case NeuV(v) => NeuV(AppN(v, eval(arg)))
        case _       => throw Error(s"expected function to apply, got ${func}")
      }
  }

  def valueToExpr(value: Value)(implicit env: Env): Expr = {
    valueToExpr_(value)(env.types.size + env.values.size + 1)
  }
  def valueToExpr_(value: Value)(implicit num: Int): Expr = value match {
    case NeuV(VarN(name)) => Var(name)
    case NeuV(AppN(func, arg)) =>
      App(valueToExpr_(NeuV(func)), valueToExpr_(arg))
    case UniV(level) => Uni(level)
    case PiV(ty, impl) =>
      val freshen = s"a'${num}"
      Pi(
        freshen,
        valueToExpr_(ty)(num),
        valueToExpr_(impl(NeuV(VarN(freshen))))(num + 1),
      )
    case LamV(ty, impl) =>
      val freshen = s"a'${num}"
      Lam(
        freshen,
        valueToExpr_(ty)(num),
        valueToExpr_(impl(NeuV(VarN(freshen))))(num + 1),
      )
    case _ => throw Error(s"expected value, got ${value}")
  }

  def normalize(expr: Expr)(implicit env: Env): Value = eval(expr)

  def typeEq(lhs: Value, rhs: Value)(implicit env: Env): Boolean = {
    (valueToExpr(lhs) == valueToExpr(rhs))
  }

  def infer(expr: Expr)(implicit env: Env): Value = {
    expr match {
      case Var(name) =>
        env.types.getOrElse(
          name,
          { throw Error(s"unknown variable ${name}") },
        )
      case Uni(lvl) => UniV(lvl + 1)
      case Pi(name, ty, body) =>
        val pLvl = typeLvl(ty)(env)
        val paramV = eval(ty)(env)
        val rLvl = typeLvl(body)(env.addVar(name, paramV))
        UniV(pLvl.max(rLvl))
      case Lam(name, ty, body) =>
        val _ = typeLvl(ty)
        val paramV = eval(ty)(env)
        val retV = infer(body)(env.addVar(name, paramV))
        val retE = valueToExpr(retV)
        PiV(paramV, (arg: Value) => eval(retE)(env.addVal(name, arg)))
      case App(func, arg) =>
        val funcV = infer(func)
        val argV = infer(arg)
        funcV match {
          case LamV(_, impl) => impl(eval(arg)(env))
          case PiV(paramV, impl) =>
            if !typeEq(paramV, argV) then
              throw Error(s"type mismatch, ${paramV} v.s. ${argV}")
            impl(eval(arg))
          case _ => throw Error(s"expected function to apply, got ${func}")
        }
    }
  }

  def typeLvl(expr: Expr)(implicit env: Env): Int = {
    infer(expr) match {
      case UniV(level) => level
      case rest        => throw Error(s"expect type, got ${rest}")
    }
  }
}

type Value = scala.Nothing;
