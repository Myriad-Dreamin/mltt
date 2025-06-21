package mltt

import syntax._

// object Subst {
//   type Env = Map[String, Expr]

//   def infer(expr: Expr)(implicit env: Env): Type = expr match {
//     case Var(name) =>
//       env.getOrElse(name, { throw Error(s"unknown variable ${name}") })
//     case Uni(lvl) => Uni(lvl + 1)
//     case Pi(name, ty, retTy) =>
//       Uni(typeLvl(ty).max(typeLvl(retTy)(env + (name -> ty))))
//     case Lam(name, ty, body @ Pi(_, _, _)) =>
//       val pty = normalize(ty)
//       Pi(name, pty, body)
//     case Lam(name, ty, body @ Uni(_)) =>
//       val pty = normalize(ty)
//       Pi(name, pty, body)
//     case Lam(name, ty, body) =>
//       val pty = normalize(ty)
//       val bodyTy = infer(body)(env + (name -> pty))
//       Pi(name, pty, bodyTy)
//     case App(func, arg) =>
//       val (funcTy, argTy) = (infer(func), infer(arg))
//       normalize(funcTy) match {
//         case Pi(name, paramTy, retTy) =>
//           if !typeEq(paramTy, argTy) then
//             throw Error(
//               s"type mismatch (${name}), ${paramTy} v.s. ${arg} (${argTy})",
//             )
//           normalize(subst(name, arg, retTy))
//         case ty => throw Error(s"expect function to call, got ${ty}")
//       }
//   }
//   def isType(expr: Expr)(implicit env: Env): Type = { typeLvl(expr); expr }
//   def typeLvl(expr: Expr)(implicit env: Env): Int = {
//     normalize(infer(expr)) match {
//       case Uni(level) => level
//       case Pi(name, ty, body) =>
//         val tyLvl = typeLvl(ty)(env + (name -> ty))
//         val bodyLvl = typeLvl(body)(env + (name -> ty))
//         tyLvl.max(bodyLvl)
//       case Var(name) =>
//         env.get(name) match {
//           case Some(ty) => typeLvl(ty)
//           case None     => throw Error(s"unknown variable ${name}")
//         }
//       case rest => throw Error(s"expect type, got ${rest}")
//     }
//   }
//   def typeEq(lhs: Type, rhs: Type)(implicit env: Env) =
//     (αEquiv(normalize(lhs), normalize(rhs))(Map()));
//   def αEquiv(expr: Expr, other: Expr)(implicit
//       names: Map[String, String],
//   ): Boolean = {
//     (expr, other) match {
//       case (Var(name1), Var(name2)) =>
//         names.getOrElse(name1, name1).contains(name2)
//       case (Uni(lvl1), Uni(lvl2)) => lvl1 == lvl2
//       case (Pi(name1, ty1, body1), Pi(name2, ty2, body2)) =>
//         αEquiv(ty1, ty2) && αEquiv(body1, body2)(names + (name1 -> name2))
//       case (Lam(name1, ty1, body1), Lam(name2, ty2, body2)) =>
//         αEquiv(ty1, ty2) && αEquiv(body1, body2)(names + (name1 -> name2))
//       case (App(func1, arg1), App(func2, arg2)) =>
//         αEquiv(func1, func2) && αEquiv(arg1, arg2)
//       case _ => false
//     }
//   }
//   def subst(n: String, into: Expr, expr: Expr): Expr = expr match {
//     case Var(name) if name == n => into
//     case Var(_)                 => expr
//     case Uni(lvl)               => Uni(lvl)
//     case Pi(name, ty, body) =>
//       if name != n then Pi(name, subst(n, into, ty), subst(n, into, body))
//       else
//         val freshen = s"a'${countVars(into, expr) + 1}"
//         val fresh = subst(freshen, Var(freshen), _)
//         Pi(freshen, subst(n, into, fresh(ty)), subst(n, into, fresh(body)))
//     case Lam(name, ty, body) =>
//       if name != n then Pi(name, subst(n, into, ty), subst(n, into, body))
//       else
//         val freshen = s"a'${countVars(into, expr) + 1}"
//         val fresh = subst(freshen, Var(freshen), _)
//         Pi(freshen, subst(n, into, fresh(ty)), subst(n, into, fresh(body)))
//     case App(func, arg) =>
//       App(subst(n, into, func), subst(n, into, arg))
//   }
//   def countVars(into: Expr, expr: Expr): Int = expr match {
//     case Var(_) => 1
//     case Uni(_) => 0
//     case Pi(name, ty, body) =>
//       countVars(into, ty) + countVars(into, body)
//     case Lam(name, ty, body) =>
//       countVars(into, ty) + countVars(into, body)
//     case App(func, arg) =>
//       countVars(into, func) + countVars(into, arg)
//   }
//   def normalize(expr: Expr)(implicit env: Env): Expr = {
//     expr match {
//       case Var(name) => Var(name)
//       case Uni(lvl)  => Uni(lvl)
//       case Pi(name, ty, body) =>
//         Pi(name, normalize(ty), normalize(body))
//       case Lam(name, ty, body) =>
//         Lam(name, normalize(ty), normalize(body))
//       case App(func, arg) =>
//         normalize(func) match {
//           case Var(name) =>
//             env.get(name) match {
//               case Some(ty) => normalize(App(ty, arg))
//               case None     => App(func, normalize(arg))
//             }
//           case Lam(name, _, retTy) =>
//             normalize(subst(name, arg, retTy))
//           case Pi(name, _, retTy) =>
//             normalize(subst(name, arg, retTy))
//           case func => App(func, normalize(arg))
//         }
//     }
//   }
// }

// object NBE {
//   type VEnv = Map[String, Value]
//   case class Env(
//       types: VEnv = Map(),
//       values: VEnv = Map(),
//   ) {
//     def addVal(name: String, value: Value): Env = {
//       this.copy(values = values + (name -> value))
//     }
//     def addVar(name: String, ty: Value): Env = {
//       Env(types + (name -> ty), values + (name -> NeuV(VarN(name))))
//     }
//   }

//   sealed abstract class Neutral;
//   case class VarN(name: String) extends Neutral;
//   case class AppN(func: Neutral, arg: Value) extends Neutral;
//   sealed abstract class Value;
//   case class NeuV(neutral: Neutral) extends Value;
//   case class UniV(level: Int) extends Value;
//   case class PiV(ty: Value, impl: Value => Value) extends Value;
//   case class LamV(ty: Value, impl: Value => Value) extends Value;

//   def eval(expr: Expr)(implicit env: Env): Value = expr match {
//     case Var(name) =>
//       env.values.getOrElse(
//         name,
//         { throw Error(s"unknown variable ${name}") },
//       )
//     case Uni(lvl) => UniV(lvl)
//     case Pi(name, ty, body) =>
//       PiV(eval(ty), (arg: Value) => eval(body)(env.addVal(name, arg)))
//     case Lam(name, ty, body) =>
//       LamV(eval(ty), (arg: Value) => eval(body)(env.addVal(name, arg)))
//     case App(func, arg) =>
//       eval(func) match {
//         case LamV(_, impl) => impl(eval(arg))
//         case PiV(_, impl)  => impl(eval(arg))
//         case NeuV(VarN(name)) =>
//           env.types.getOrElse(
//             name,
//             { throw Error(s"unknown variable ${name}") },
//           ) match {
//             case LamV(_, impl) => impl(eval(arg))
//             case PiV(_, impl)  => impl(eval(arg))
//             case NeuV(neutral) => NeuV(AppN(neutral, eval(arg)))
//             case _ => throw Error(s"expected function to apply, got ${func}")
//           }
//         case NeuV(v) => NeuV(AppN(v, eval(arg)))
//         case _       => throw Error(s"expected function to apply, got ${func}")
//       }
//   }

//   def valueToExpr(value: Value)(implicit env: Env): Expr = {
//     valueToExpr_(value)(env.types.size + env.values.size + 1)
//   }
//   def valueToExpr_(value: Value)(implicit num: Int): Expr = value match {
//     case NeuV(VarN(name)) => Var(name)
//     case NeuV(AppN(func, arg)) =>
//       App(valueToExpr_(NeuV(func)), valueToExpr_(arg))
//     case UniV(level) => Uni(level)
//     case PiV(ty, impl) =>
//       val freshen = s"a'${num}"
//       Pi(
//         freshen,
//         valueToExpr_(ty)(num),
//         valueToExpr_(impl(NeuV(VarN(freshen))))(num + 1),
//       )
//     case LamV(ty, impl) =>
//       val freshen = s"a'${num}"
//       Lam(
//         freshen,
//         valueToExpr_(ty)(num),
//         valueToExpr_(impl(NeuV(VarN(freshen))))(num + 1),
//       )
//     case _ => throw Error(s"expected value, got ${value}")
//   }

//   def normalize(expr: Expr)(implicit env: Env): Value = eval(expr)

//   def typeEq(lhs: Value, rhs: Value)(implicit env: Env): Boolean = {
//     (valueToExpr(lhs) == valueToExpr(rhs))
//   }

//   def infer(expr: Expr)(implicit env: Env): Value = {
//     expr match {
//       case Var(name) =>
//         env.types.getOrElse(
//           name,
//           { throw Error(s"unknown variable ${name}") },
//         )
//       case Uni(lvl) => UniV(lvl + 1)
//       case Pi(name, ty, body) =>
//         val pLvl = typeLvl(ty)(env)
//         val paramV = eval(ty)(env)
//         val rLvl = typeLvl(body)(env.addVar(name, paramV))
//         UniV(pLvl.max(rLvl))
//       case Lam(name, ty, body) =>
//         val _ = typeLvl(ty)
//         val paramV = eval(ty)(env)
//         val retV = infer(body)(env.addVar(name, paramV))
//         val retE = valueToExpr(retV)
//         PiV(paramV, (arg: Value) => eval(retE)(env.addVal(name, arg)))
//       case App(func, arg) =>
//         val funcV = infer(func)
//         val argV = infer(arg)
//         funcV match {
//           case LamV(_, impl) => impl(eval(arg))
//           case PiV(_, impl)  => impl(eval(arg))
//           case _ => throw Error(s"expected function to apply, got ${func}")
//         }
//     }
//   }

//   def typeLvl(expr: Expr)(implicit env: Env): Int = {
//     infer(expr) match {
//       case UniV(level) => level
//       case rest        => throw Error(s"expect type, got ${rest}")
//     }
//   }
// }

class Lyzh {

  sealed abstract class Term;
  case class Cons(name: String, lvl: Int) extends Term;
  case class Var(name: String) extends Term;
  case class Apply(func: Term, arg: Term) extends Term;
  case object Uni extends Term;
  case object UniP extends Term;
  case class Def(name: String, ty: Term, body: Term, lvl: Boolean)
      extends Term {
    override def toString: String = {
      val it = if lvl then "Pi" else "Lam"
      s"$it[(${name}: ${ty}) => ${body}]"
    }
  }

  enum DefTerm {
    case Local(term: Term)
    case Global(v: Term, t: Term)

    def value(name: String): Term = this match {
      case Local(term)  => Var(name)
      case Global(v, _) => v
    }

    def ty: Term = this match {
      case Local(term)  => term
      case Global(_, t) => t
    }
  }
  import DefTerm._

  type Env = Map[String, DefTerm]

  def tyck(e: Env, x: Let): Env = x match {
    case Let(_, name, annotation, init) =>
      implicit val env: Env = e
      val annoTy = annotation.map(check(_, Uni)).getOrElse(Uni)
      val let = globalLet(name, init, annoTy)
      println(s"checking ${name}, $let")
      (env + (name -> let))
  }

  var freshenCount = 0;
  def freshName(name: String): String = {
    freshenCount += 1
    if name.endsWith("'") then
      // remove the old freshen count.
      val primIdx = name.dropRight(1).lastIndexOf('@')
      s"${name.take(primIdx)}@${freshenCount}'"
    else s"${name}@${freshenCount}'"
  }

  def unresolved(name: String): Nothing = {
    throw Error(s"unresolved variable ${name}")
  }

  def resolve(name: String)(implicit env: Env) =
    env.getOrElse(name, unresolved(name)).ty

  inline def let(name: String, term: Term)(implicit env: Env): Env =
    println(s"let ${name} = ${term}")
    env + (name -> Local(term))

  def globalLet(name: String, expr: Expr, anno: Term)(implicit env: Env) =
    expr match {
      case _: Lam =>
        check(expr, anno) match {
          case term @ Def(name, ty, body, lvl) =>
            Global(
              term,
              eval(anno) match {
                case v @ Def(name, ty, body, lvl) => v
                case _                            => lift(term)
              },
            )
          case _ => {
            throw Error(s"expected function type, got ${expr}")
          }
        }
      case Ext(name) => Global(Cons(name, 1), Cons(name, 2))
      case expr      => Global(check(expr, anno), anno)
    }

  def lift(term: Term)(implicit env: Env): Term = term match {
    case Var(name)        => Var(name)
    case Cons(name, lvl)  => Cons(name, lvl)
    case Uni | UniP       => term
    case Apply(func, arg) => Apply(lift(func), lift(arg))
    case Def(name, ty, body, lvl) =>
      Def(name, lift(ty), lift(body)(let(name, ty)), true)
  }

  def subst(term: Term)(implicit name: String, into: Term): Term = term match {
    case Var(n) if n == name              => into
    case Cons(_, _) | Uni | UniP | Var(_) => term
    case Apply(func, arg) =>
      Apply(subst(func), subst(arg))
    case Def(n, ty, body, lvl) =>
      if n != name then Def(n, subst(ty), subst(body), lvl)
      else {
        val freshen = freshName(n)
        def subst2(term: Term) = subst(term)(freshen, Var(freshen))
        Def(freshen, subst(subst2(ty)), subst(subst2(body)), lvl)
      }
  }

  def check(expr: Expr, expectedT: Term)(implicit env: Env): Term =
    expr match {
      case Lam(name, paramE, bodyE, lvl) =>
        eval(expectedT) match {
          case Def(name, param, bodyTerm, lvl) =>
            val bodyTy = subst(bodyTerm)(name, Var(name))
            Def(name, param, check(bodyE, bodyTy)(let(name, param)), lvl)
          case Uni | UniP =>
            val (param, paramTy) = infer(paramE)
            Def(name, param, check(bodyE, UniP)(let(name, param)), lvl)
          case ty => throw Error(s"expected function type, got ${ty}")
        }
      case _: Todo =>
        eval(expectedT) match {
          case Cons(name, lvl) => Cons(name, lvl - 1)
          case v               => throw Error(s"expected type, got ${v}")
        }
      case expr =>
        val (valT, tyT) = infer(expr)
        println(s"checking ${expr} : ${valT} : ${tyT} against ${expectedT}")
        val (fact, presume) = (eval(tyT), eval(expectedT))
        if !unify(fact, presume) then
          throw Error(s"type mismatch, expected ${presume}, got ${fact}")
        valT
    }

  def infer(expr: Expr)(implicit env: Env): (Term, Term) = expr match {
    case Name(name) =>
      val item = env.getOrElse(name, unresolved(name))
      (item.value(name), item.ty)
    case Ext(name) => (Cons(name, 1), Cons(name, 2))
    case _: Todo   => ???
    case UniE(_)   => (Uni, Uni)
    case Lam(name, paramE, bodyE, lvl) =>
      val param = check(paramE, Uni)
      val bodyTy = check(bodyE, Uni)(let(name, param))
      val ty = if lvl then Uni else Def(name, param, bodyTy, true)
      (Def(name, param, bodyTy, lvl), ty)
    case App(func, arg) =>
      val (funcTerm, funcTy) = infer(func)(env)
      funcTy match {
        case Def(name, param, bodyTy, lvl) =>
          val argTerm = check(arg, param)(let(name, param))
          val retTy = subst(bodyTy)(name, argTerm)
          val retTerm = opApply(funcTerm, argTerm)
          println(s"applying got ${retTerm} : ${retTy}")
          (retTerm, retTy)
        case _ => throw Error(s"expected function to apply, got ${funcTerm}")
      }
  }

  def unify(lhs: Term, rhs: Term)(implicit env: Env): Boolean =
    (lhs, rhs) match {
      case (u: Var, v: Var)         => u == v
      case (u: Cons, v: Cons)       => u == v
      case (_, UniP)                => true
      case ((_: (Cons) | Uni), Uni) => true
      case (Apply(func1, arg1), Apply(func2, arg2)) =>
        unify(func1, func2) && unify(arg1, arg2)
      case (Def(nameL, tyL, bodyL, ll), Def(nameR, tyR, bodyR, lr))
          if ll == lr =>
        // todo: check ty
        unify(bodyL, subst(bodyR)(nameR, Var(nameL)))
      case _ => false
    }

  def opApply(func: Term, arg: Term)(implicit env: Env): Term = {
    func match {
      case Def(name, _, body, _) => subst(body)(name, arg)
      case _                     => Apply(func, arg)
    }
  }

  def eval(term: Term)(implicit env: Env): Term = term match {
    case Cons(_, _) | Uni | UniP => term
    case Var(name) => rename(env.get(name).map(_.value(name)).getOrElse(term))
    case Def(name, ty, body, lvl) => Def(name, eval(ty), eval(body), lvl)
    case Apply(func, arg) =>
      val (funcVal, argVal) = (eval(func), eval(arg))
      funcVal match {
        case Def(name, _, body, _) => subst(body)(name, argVal)
        case _                     => Apply(funcVal, argVal)
      }
  }

  def rename(term: Term): Term =
    def go(term: Term)(implicit env: Env): Term = term match {
      case Var(name) => env.get(name).map(_.value(name)).getOrElse(term)
      case Cons(_, _) | Uni | UniP => term
      case Def(name, ty, body, lvl) =>
        val renameEnv = let(name, Var(freshName(name)))
        Def(name, go(ty)(renameEnv), go(body)(renameEnv), lvl)
      case Apply(func, arg) =>
        Apply(go(func), go(arg))
    }
    go(term)(Map())
}

type Value = scala.Nothing;
