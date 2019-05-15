
/////// ----- Preliminary ----- /////////

@FunctionalInterface
trait Show[A] {
  /**Used for converting values to a String for printing
  */
  def (a: A) show: String
}

object Show {
  /**Displays a value given an instance of Show.
  */
  def puts[A](a: A) given Show[A] =
    println(the[Show[A]].show(a))
}

import Show._
import annotation.tailrec

implied for Show[String] = identity

/////// ----- Trees ----- /////////

type Name = String

opaque type Constant = Any

object Constant {
  def apply(a: Any): Constant = a.asInstanceOf[Constant]
  def (c: Constant) render: String = c.toString
  implied for Eql[Constant, Constant] = Eql.derived
}

import Constant._

enum Ops derives Eql {
  case Fst, Snd, Inl, Inr
}

import Ops._

enum Tree derives Eql {
  case Point
  case Pair(a: Tree, b: Tree)
  case Var(name: Name)
  case App(f: Tree, t: Tree)
  case Eval(f: Tree, t: Tree)
  case Lam(x: Name, t: Tree)
  case Lin(x: Name, t: Tree)
  case Lit(c: Constant)
  case CaseExpr(e: Tree, x: Name, l: Tree, y: Name, r: Tree)
  case Op(ops: Ops)
  case Bang(t: Tree)
  case WhyNot(e: Tree)
  case Tensor(t: Tree, z: Tree)
  case Let(n: Name, t: Tree, u: Tree)
  case LetT(x: Name, z: Name, s: Tree, t: Tree)
}

import Tree._

def (tree: Tree) fold[A](z: A)(f: (A, Tree) => A): A = {
  @tailrec
  def inner(z: A, ts: List[Tree]): A = ts match {
    case Nil => z
    case t::ts => t match {
      case Pair(a,b)            => inner(f(z,t), a::b::ts)
      case App(g,u)             => inner(f(z,t), g::u::ts)
      case Eval(g,u)            => inner(f(z,t), g::u::ts)
      case Lam(_,u)             => inner(f(z,t), u::ts)
      case Lin(_,u)             => inner(f(z,t), u::ts)
      case CaseExpr(e,x,l,y,r)  => inner(f(z,t), e::l::r::ts)
      case Bang(u)              => inner(f(z,t), u::ts)
      case WhyNot(u)            => inner(f(z,t), u::ts)
      case Tensor(v,u)          => inner(f(z,t), v::u::ts)
      case Let(_,e,u)           => inner(f(z,t), e::u::ts)
      case LetT(_,_,e,u)        => inner(f(z,t), e::u::ts)
      case _                    => inner(f(z,t), ts)
    }
  }
  inner(z, tree::Nil)
}

def (tree: Tree) pretty: String = {

  type StackT = List[String]
  type StatT = StackT => StackT
  type ProgT = List[StatT]

  def wrapIfComplex(t: Tree, s: String) = t match {
    case Point | _:(Var | Lit | Pair) => s
    case _ => s"($s)"
  }

  val prog = tree.fold(List.empty[StatT]) { (acc, t) =>
    t match {
      case Pair(a,b) =>
        { s => val a1::b1::s1 = s
          s"($a1, $b1)"::s1
        }::acc

      case Tensor(v,z) =>
        { s => val v1::z1::s1 = s
          val v2 = wrapIfComplex(v,v1)
          val z2 = wrapIfComplex(z,z1)
          s"!$v2 *: $z2"::s1
        }::acc

      case App(f,t) =>
        { s => val f1::t1::s1 = s
          val t2 = wrapIfComplex(t,t1)
          s"$f1 $t2"::s1
        }::acc

      case Eval(f,t) =>
        { s => val f1::t1::s1 = s
          s"$f1[$t1]"::s1
        }::acc

      case Lam(x,t) =>
        { s => val t1::s1 = s
          s"\\$x.$t1"::s1
        }::acc

      case Lin(x,t) =>
        { s => val t1::s1 = s
          s"^\\$x.$t1"::s1
        }::acc

      case CaseExpr(e,x,l,y,r) =>
        { s => val e1::l1::r1::s1 = s
          s"case $e1 of {inl $x.$l1; inr $y.$r1}"::s1
        }::acc

      case Let(x,t,u) =>
        { s => val t1::u1::s1 = s
          s"let !$x be $t1 in $u1"::s1
        }::acc

      case LetT(x,z,t,u) =>
        { s => val t1::u1::s1 = s
          s"let !$x *: $z be $t1 in $u1"::s1
        }::acc

      case Bang(t) =>
        { s => val t1::s1 = s
          val t2 = wrapIfComplex(t,t1)
          s"!$t2"::s1
        }::acc

      case WhyNot(t) =>
        { s => val t1::s1 = s
          val t2 = wrapIfComplex(t,t1)
          s"?$t2"::s1
        }::acc

      case Op(ops)  => (ops.toString.toLowerCase::_)::acc
      case Point    => ("*"::_)::acc
      case Var(n)   => (s"$n"::_)::acc
      case Lit(c)   => (c.render::_)::acc
    }
  }

  prog.foldLeft(List.empty[String])((a,f) => f(a)).head
}

implied for Show[Tree] = pretty

/////// ----- Types ----- /////////

enum Prim derives Eql {
  case PVoid
}

import Prim._

opaque type TName = String

object TName {
  def apply(s: String): TName = s.asInstanceOf[TName]
}

enum Type derives Eql {
  case TArr(a: Type, b: Type)
  case TLin(a: Type, b: Type)
  case TSum(a: Type, b: Type)
  case TTen(a: Type, b: Type)
  case TBang(a: Type)
  case TPair(a: Type, b: Type)
  case TBase(p: Prim)
  case TVar(n: TName)
  case TPoint
}

import Type._

val TVoid  = TBase(PVoid)

enum Scheme derives Eql { case Forall(vs: List[TName], t: Type) }

import Scheme._

/////// ----- Substitutions ----- /////////

opaque type Id = Long

object Id {
  private inline def (id: Id) asScala: Long = id.asInstanceOf[Long]
  private inline def apply(l: Long): Id     = l.asInstanceOf[Id]

  val initId = 0l

  def (id: Id) succ: Id = Id { id.asScala + 1l }
}

import Id._

trait TypeEnv[Env] {
  val empty: Env
  def (e: Env) elems: List[Scheme]
  def (e: Env) map(f: Scheme => Scheme): Env
  def (e: Env) extend(p: (Name, Scheme)): Env
  def (e: Env) restrict(x: Name): Env
}

type TypeEnvMap = Map[Name, Scheme]

implied for TypeEnv[TypeEnvMap] {
  import scala.collection.immutable.ListMap

  val empty                                     = ListMap.empty
  def (e: TypeEnvMap) map(f: Scheme => Scheme)  = e.mapValues(f)
  def (e: TypeEnvMap) extend(p: (Name, Scheme)) = e + p
  def (e: TypeEnvMap) restrict(x: Name)         = e - x
  def (e: TypeEnvMap) elems = e.toSeq.view.sortBy(_._1).map(_._2).toList
}

enum TypeError { case Error(msg: String) }

import TypeError._

type Except[A] = Either[TypeError, A]

type Subst = Map[TName, Type]

trait Substitutable[A] {
  def (a: A) `subFrom` (s: Subst): A
  def (a: A) free: Set[TName]
}

implied for Substitutable[Type] {
  def (a: Type) `subFrom` (s: Subst) = a match {
    case t @ (_:TBase | TPoint) => t
    case t @ TVar(a)            => s.getOrElse(a, t)
    case t1 `TArr` t2           => TArr(t1 `subFrom` s, t2 `subFrom` s)
    case t1 `TLin` t2           => TLin(t1 `subFrom` s, t2 `subFrom` s)
    case TTen(t1,t2)            => TTen(t1 `subFrom` s, t2 `subFrom` s)
    case TSum(t1,t2)            => TSum(t1 `subFrom` s, t2 `subFrom` s)
    case TPair(t1,t2)           => TPair(t1 `subFrom` s, t2 `subFrom` s)
    case TBang(t)               => TBang(t `subFrom` s)
  }

  def (a: Type) free: Set[TName] = a match {
    case _:TBase | TPoint => Set.empty
    case TVar(a)          => Set(a)
    case t1 `TArr` t2     => t1.free ++ t2.free
    case t1 `TLin` t2     => t1.free ++ t2.free
    case TTen(t1,t2)      => t1.free ++ t2.free
    case TSum(t1,t2)      => t1.free ++ t2.free
    case TPair(t1,t2)     => t1.free ++ t2.free
    case TBang(t)         => t.free
  }
}

type State[S, A] = S => (A, S)

trait Functor[F[_]] {
  def (fa: F[A]) map[A, B](f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def (fa: F[A]) flatMap[A, B](f: A => F[B]): F[B]
}

trait StateF[S] extends Functor[[A] => State[S,A]] {
  def (sa: State[S,A]) map[A, B](f: A => B): State[S,B] = { state =>
    val (a, s) = sa(state)
    (f(a), s)
  }
}

trait StateM[S] extends StateF[S] with Monad[[A] => State[S,A]] {

  def pure[A](a: A): State[S,A] = state => (a, state)

  def (sa: State[S,A]) flatMap[A, B](f: A => State[S,B]): State[S,B] =
    { state =>
      val (a, s1) = sa(state)
      f(a)(s1)
    }
}

implied [S] for StateF[S]

val letters: Stream[String] = {
  import language.implicitConversions
  val alpha = ('a' to 'z').toStream.map(_.toString)
  val numeric = for
    n <- Stream.from(1)
    x <- alpha
  yield s"$x$n"
  alpha #::: numeric
}

implied for Substitutable[Scheme] {
  def (a: Scheme) `subFrom` (s: Subst) = {
    val Forall(as, t) = a
    val free = as.foldLeft(s)(_ - _)
    Forall(as, t `subFrom` free)
  }

  def (a: Scheme) free: Set[TName] = {
    val Forall(as, t) = a
    t.free -- as
  }
}

implied [A:Substitutable] for Substitutable[List[A]] {
  def (as: List[A]) `subFrom` (s: Subst) = as.map(_ `subFrom` s)
  def (as: List[A]) free                 = as.foldLeft(Set.empty)(_ ++ _.free)
}

implied [Env:TypeEnv] for Substitutable[Env] {
  def (e: Env) `subFrom` (s: Subst) = e.map(_ `subFrom` s)
  def (a: Env) free                 = a.elems.free
}

val emptySubst: Subst = Map.empty

def (s1: Subst) `compose` (s2: Subst) = (s2 ++ s1) `mapValues` (_ `subFrom` s1)

/////// ----- Parsing ----- /////////

object Parsing {
  import language.implicitConversions
  import util.parsing.combinator._

  private object EECParsers extends JavaTokenParsers {
    type P = Parser[Tree]

    def expr:  P = lam | lin | let | letT | cse | app
    def expr1: P = tsor | pExpr
    def pExpr: P = op | aexpr
    def aexpr: P = ref | unit | pair | defr

    def app: P = rep1(expr1)       ^^ { case ts => ts.reduceLeft(App(_,_)) }
    def lam: P = "\\"~id~"."~expr  ^^ { case _~x~_~t => Lam(x,t) }
    def lin: P = "^\\"~id~"."~expr ^^ { case _~x~_~t => Lin(x,t) }

    def let: P = "let"~"!"~id~"be"~expr~"in"~expr ^^ {
      case _~_~x~_~t~_~u => Let(x,t,u)
    }

    def letT: P = "let"~"!"~id~"*:"~id~"be"~expr~"in"~expr ^^ {
      case _~_~x~_~y~_~s~_~t => LetT(x,y,s,t)
    }

    def cse: P = {
      "case"~expr~"of"~"{"~
        "inl"~id~"."~expr~";"~
        "inr"~id~"."~expr~
      "}" ^^ { case _~e~_~_~_~x~_~l~_~_~y~_~r~_ => CaseExpr(e,x,l,y,r) }
    }

    def op: P = ("!" | "?" | "fst" | "snd" | "inl" | "inr")~aexpr ^^ {
      case op~e => op match {
        case "!"   => Bang(e)
        case "?"   => WhyNot(e)
        case "fst" => App(Op(Fst),e)
        case "snd" => App(Op(Snd),e)
        case "inl" => App(Op(Inl),e)
        case "inr" => App(Op(Inr),e)
      }
    }

    def tsor: P = "!"~aexpr~"*:"~expr1  ^^ { case _~t~_~z   => Tensor(t,z) }
    def pair: P = "("~expr~","~expr~")" ^^ { case _~t~_~u~_ => Pair(t,u) }
    def defr: P = "("~expr~")"          ^^ { case _~e~_     => e }
    def unit: P = "*"                   ^^ { _              => Point }
    def ref:  P = id                    ^^ {                   Var(_) }

    val reservedWords = Set(
      "case", "of", "let", "be", "in", "fst", "snd", "inl", "inr"
    )

    val id = Parser { input =>
      ident(input).filterWithError(
        !reservedWords.contains(_),
        reservedWord => s"inappropriate use of $reservedWord",
        input
      )
    }
  }

  object EEC {
    import EECParsers._
    import java.io.StringReader

    def (s: String) eec: Tree | String = parseAll(expr, StringReader(s)) match {
      case Success(matched,_) => matched
      case f:Failure          => f.toString
      case e:Error            => e.toString
    }

    def (r: Tree | String) ! = r match {
      case matched: Tree => matched
      case err: String   => sys.error(err)
    }
  }
}

import Parsing.EEC._

puts("let !x *: z be s in inr".eec.!)
puts("let !y be x in *".eec.!)
puts("!a".eec.!)
puts("?v".eec.!)
puts("a".eec.!)
puts("fst (a, b)".eec.!)
puts("snd (*, (a, b))".eec.!)
puts("inl *".eec.!)
puts("inr *".eec.!)
puts("!a *: b".eec.!)
puts("f (a b) (c d e)".eec.!)
puts("case z of {inl l. f a b; inr r. *}".eec.!)

/////// ----- Usage ----- /////////

val I = """ \x.x """.eec.!
val K = """ \x.\y.x """.eec.!
val S = """ \f.\g.\x.f x (g x) """.eec.!

val Bind = """ \f.\x.let !y be x in f y """.eec.!

puts(I)
puts(K)
puts(S)

puts(Bind)