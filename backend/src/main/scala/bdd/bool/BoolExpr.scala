// backend/src/main/scala/bool/BoolExpr.scala
object BoolExpr {

  sealed trait Expr
  final case class Var(name: String) extends Expr
  final case class Const(value: Boolean) extends Expr

  final case class Not(e: Expr) extends Expr
  final case class And(a: Expr, b: Expr) extends Expr
  final case class Nand(a: Expr, b: Expr) extends Expr
  final case class Or(a: Expr, b: Expr) extends Expr
  final case class Nor(a: Expr, b: Expr) extends Expr
  final case class Xor(a: Expr, b: Expr) extends Expr
  final case class Implies(a: Expr, b: Expr) extends Expr
  final case class Iff(a: Expr, b: Expr) extends Expr

  def pretty(e: Expr): String = {
    def fully(expr: Expr): String = expr match {
      case Var(name)   => name
      case Const(true) => "1"
      case Const(false) => "0"

      case Not(inner) =>
        inner match {
          case Var(_) | Const(_) | Not(_) => s"¬${fully(inner)}"
          case _ =>
            val s = fully(inner)
            s"¬${s}"
        }

      case And(a, b) =>
        s"(${fully(a)} ∧ ${fully(b)})"

      case Nand(a, b) =>
        s"(${fully(a)} ↑ ${fully(b)})"

      case Xor(a, b) =>
        s"(${fully(a)} ⊕ ${fully(b)})"

      case Or(a, b) =>
        s"(${fully(a)} ∨ ${fully(b)})"

      case Nor(a, b) =>
        s"(${fully(a)} ↓ ${fully(b)})"

      case Implies(a, b) =>
        s"(${fully(a)} → ${fully(b)})"

      case Iff(a, b) =>
        s"(${fully(a)} ↔ ${fully(b)})"
    }

    e match {
      case Var(_) | Const(_) | Not(_) => fully(e)
      case _ =>
        val out = fully(e)
        out.substring(1, out.length - 1)
    }
  }

  def eval(e: Expr, env: Map[String, Boolean]): Boolean = e match {
    case Var(x)        => env(x)
    case Const(v)      => v
    case Not(inner)    => !eval(inner, env)
    case And(a, b)     => eval(a, env) && eval(b, env)
    case Nand(a, b)    => !(eval(a, env) && eval(b, env))
    case Or(a, b)      => eval(a, env) || eval(b, env)
    case Nor(a, b)     => !(eval(a, env) || eval(b, env))
    case Xor(a, b)     => eval(a, env) ^ eval(b, env)
    case Implies(a, b) => !eval(a, env) || eval(b, env)
    case Iff(a, b)     => eval(a, env) == eval(b, env)
  }

  def collectVars(e: Expr): Set[String] = e match {
    case Var(x)        => Set(x)
    case Const(_)      => Set.empty
    case Not(i)        => collectVars(i)
    case And(a, b)     => collectVars(a) ++ collectVars(b)
    case Nand(a, b)    => collectVars(a) ++ collectVars(b)
    case Or(a, b)      => collectVars(a) ++ collectVars(b)
    case Nor(a, b)     => collectVars(a) ++ collectVars(b)
    case Xor(a, b)     => collectVars(a) ++ collectVars(b)
    case Implies(a, b) => collectVars(a) ++ collectVars(b)
    case Iff(a, b)     => collectVars(a) ++ collectVars(b)
  }

  def truthTable(e: Expr, vars: List[String]): List[(Map[String, Boolean], Boolean)] = {
    val need = collectVars(e)
    val missing = need -- vars.toSet
    require(missing.isEmpty, s"vars is missing variables: ${missing.mkString(", ")}")

    val envs = allEnvsInOrder(vars)
    envs.map(env => env -> eval(e, env))
  }

  private def allEnvsInOrder(vars: List[String]): List[Map[String, Boolean]] =
    vars match {
      case Nil => List(Map.empty)
      case v :: vs =>
        val rest = allEnvsInOrder(vs)
        rest.map(_ + (v -> true)) ++ rest.map(_ + (v -> false))
    }
}
