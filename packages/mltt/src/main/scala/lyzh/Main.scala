package lyzh

import syntax._

class Lyzh {

  /// A well-formed item identified by `Lyzh`.
  sealed abstract class Term;
  /// Type Universe
  case object Uni extends Term;
  /// Variable Reference
  case class Var(name: String) extends Term;
  /// Function Application
  case class Apply(func: Term, arg: Term) extends Term;
  /// Function Type or Lambda
  /// If `lvl` is true, it is a Pi type (dependent function type),
  /// otherwise it is a Lam (regular function).
  case class Def(name: String, ty: Term, body: Term, lvl: Boolean)
      extends Term {
    override def toString: String = {
      s"${if lvl then "Pi" else "Lam"}[(${name}: ${ty}) => ${body}]"
    }
  }

  /// A local environment mapping variable names to terms.
  type Env = Map[String, Term]

  /// The checked global definitions.
  /// todo: stateful is in my flavor.
  var globals: Map[String, (Term, Term)] = Map()

  /// Type check let binding, and update the global definitions.
  def tyck(let: Let) = {
    /// Checks all parameters and adds them to the environment.
    val (envWithParams, params) = let.params.iterator.flatten
      .foldLeft((Map[String, Term](), List[(String, Term)]())) {
        case ((env, params), Param(name, ty)) =>
          val paramTy = ty.map(check(_, Uni)(env)).getOrElse(Uni)
          (env + (name -> paramTy), params :+ (name, paramTy))
      }
    /// Checks the annotation type, which must be in `Uni`.
    val annoTy = let.ty.map(check(_, Uni)(envWithParams)).get
    /// Checks the body type with user annotated `annoTy`.
    val bodyTm = check(let.init, annoTy)(envWithParams)

    def defCons(isType: Boolean) =
      params.foldRight(if isType then annoTy else bodyTm) {
        case ((name, paramTy), bodyTm) => Def(name, paramTy, bodyTm, isType)
      }
    globals = globals + (let.name -> (defCons(false), defCons(true)))
  }

  /// Makes a new name.
  var freshenCount = 0;
  def freshName(name: String): String = {
    // todo: fresh counter is stateful, which is not good.
    freshenCount += 1
    if name.endsWith("'") then
      // remove the old freshen count.
      val primIdx = name.dropRight(1).lastIndexOf('@')
      s"${name.take(primIdx)}@${freshenCount}'"
    else s"${name}@${freshenCount}'"
  }

  /// A shortcut to create a new environment with the additional local variable.
  inline def let(name: String, term: Term)(implicit env: Env): Env =
    env + (name -> term)

  /// Checks the expression against the expected type.
  def check(expr: Expr, expectedT: Term)(implicit env: Env): Term =
    expr match {
      case Lam(Param(name, paramE), bodyE, lvl) =>
        eval(expectedT) match {
          case Def(name, param, bodyTerm, _) =>
            val bodyTy = subst(bodyTerm)(name, Var(name))
            Def(name, param, check(bodyE, bodyTy)(let(name, param)), lvl)
          case Uni =>
            val (param, paramTy) = paramE.map(infer).getOrElse((Var(name), Uni))
            val body = if lvl then check(bodyE, Uni)(let(name, param)) else ???
            Def(name, param, check(bodyE, Uni)(let(name, param)), lvl)
          case ty => throw Error(s"expected function type, got ${ty}")
        }
      case expr =>
        val (valT, tyT) = infer(expr)
        val (fact, presume) = (eval(tyT), eval(expectedT))
        if !unify(fact, presume) then
          throw Error(s"type mismatch, expected ${presume}, got ${fact}")
        valT
    }

  /// Throws undefined variable error.
  def unresolved(name: String): Nothing =
    throw Error(s"unresolved variable ${name}")
  /// Infers the type of the expression.
  def infer(expr: Expr)(implicit env: Env): (Term, Term) = expr match {
    case Name(name) =>
      val localRet = env.get(name).map((Var(name), _))
      localRet.getOrElse(globals.getOrElse(name, unresolved(name)))
    case UniE(_) => (Uni, Uni)
    case Lam(Param(name, paramE), bodyE, lvl) =>
      val param = paramE.map(check(_, Uni)).getOrElse(Uni)
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
          (retTerm, retTy)
        case _ => throw Error(s"expected function to apply, got ${funcTy}")
      }
  }

  /// Unifies two terms, returning true if they are equivalent.
  def unify(lhs: Term, rhs: Term)(implicit env: Env): Boolean =
    (lhs, rhs) match {
      case (u: Var, v: Var) => u == v
      case (Uni, Uni)       => true
      case (Apply(func1, arg1), Apply(func2, arg2)) =>
        unify(func1, func2) && unify(arg1, arg2)
      case (Def(nameL, tyL, bodyL, ll), Def(nameR, tyR, bodyR, lr))
          if ll == lr =>
        // todo: check ty
        unify(bodyL, subst(bodyR)(nameR, Var(nameL)))
      case _ => false
    }

  /// Runs the function application concretely.
  def opApply(func: Term, arg: Term)(implicit env: Env): Term = {
    func match {
      case Def(name, _, body, _) => subst(body)(name, arg)
      /// Keep the function as is if it is not a Def.
      case _ => Apply(func, arg)
    }
  }

  /// Substitutes a term into another term.
  def subst(term: Term)(name: String, into: Term): Term =
    eval(term)(Map(name -> into))
  /// Evaluates a term in the given environment.
  def eval(term: Term)(implicit env: Env): Term = term match {
    case Uni                      => term
    case Var(name)                => rename(env.get(name).getOrElse(term))
    case Def(name, ty, body, lvl) => Def(name, eval(ty), eval(body), lvl)
    case Apply(func, arg) =>
      val (funcVal, argVal) = (eval(func), eval(arg))
      funcVal match {
        /// Note: we don't run `subst` here, to substitute recursively.
        case Def(name, _, body, _) => eval(body)(let(name, argVal))
        case _                     => Apply(funcVal, argVal)
      }
  }

  /// Renames variables in the term to avoid name collisions.
  def rename(term: Term): Term =
    def go(term: Term)(implicit env: Env): Term = term match {
      case Var(name) => env.get(name).getOrElse(term)
      case Uni       => term
      case Def(name, ty, body, lvl) =>
        val newName = freshName(name)
        val renameEnv = let(name, Var(newName))
        Def(newName, go(ty)(renameEnv), go(body)(renameEnv), lvl)
      case Apply(func, arg) =>
        Apply(go(func), go(arg))
    }
    go(term)(Map())
}

type Value = scala.Nothing;
