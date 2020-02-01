
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
  def puts[A](a: A)(given Show[A])=
    println(summon[Show[A]].show(a))
}

import Show._
import annotation.tailrec

given Show[String] = identity

/////// ----- Trees ----- /////////

type Name = String

enum Ops derives Eql {
  case Fst, Snd, Inl, Inr
}

import Ops._

object ast {

  import Constants.Constant

  object Constants {
    opaque type Constant = Any

    object Constant {
      def apply(a: Any): Constant = a.asInstanceOf[Constant]
      def (c: Constant) render: String = c.toString
      given Eql[Constant, Constant] = Eql.derived
    }
  }

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
}


import ast.Tree
import ast.Constants

def [A](tree: Tree) fold(z: A)(f: (A, Tree) => A): A = {
  import Tree._

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
  import Tree._, Constants.Constant._

  type StackT = List[String]
  type StatT = StackT => StackT
  type ProgT = List[StatT]

  def wrapIfComplex(t: Tree, s: String) = t match {
    case Point | _:(Var | Lit | Pair) => s
    case _                            => s"($s)"
  }

  def wrapIfExpr1(t: Tree, s: String) = t match {
    case _:(Lam | Lin | Let | LetT | CaseExpr) => s"($s)"
    case _                                     => s
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
          val f2 = wrapIfExpr1(f,f1)
          val t2 = wrapIfComplex(t,t1)
          s"$f2 $t2"::s1
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

given Show[Tree] = pretty

/////// ----- Types ----- /////////


object Types {

  import Type._, Prim._

  enum Prim derives Eql {
    case PVoid
  }

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

  enum Scheme derives Eql { case Forall(vs: List[TName], t: Type) }

  val TVoid  = TBase(PVoid)
}


/////// ----- Substitutions ----- /////////

object Ids {
  opaque type Id = Long

  object Id {
    private def (id: Id) asScala: Long = id.asInstanceOf[Long]
    private def apply(l: Long): Id     = l.asInstanceOf[Id]

    val initId = 0l

    def (id: Id) succ: Id = Id { id.asScala + 1l }
  }
}

import Ids.Id._

object Environment {

  import Types._

  trait TypeEnv[Env] {
    val empty: Env
    def (e: Env) elems: List[Scheme]
    def (e: Env) map(f: Scheme => Scheme): Env
    def (e: Env) extend(p: (Name, Scheme)): Env
    def (e: Env) restrict(x: Name): Env
  }

  type TypeEnvMap = Map[Name, Scheme]

  given TypeEnv[TypeEnvMap] {
    import scala.collection.immutable.ListMap

    val empty                                     = ListMap.empty
    def (e: TypeEnvMap) map(f: Scheme => Scheme)  = e.view.mapValues(f).toMap
    def (e: TypeEnvMap) extend(p: (Name, Scheme)) = e + p
    def (e: TypeEnvMap) restrict(x: Name)         = e - x
    def (e: TypeEnvMap) elems = e.toSeq.view.sortBy(_._1).map(_._2).toList
  }
}


enum TypeError { case Error(msg: String) }

import TypeError._

type Except[A] = Either[TypeError, A]


object Substitutions {
  import Types._
  import Type._
  import Environment._
  import Scheme._

  type Subst = Map[TName, Type]

  val emptySubst: Subst = Map.empty

  trait Substitutable[A] {
    def (a: A) `subFrom` (s: Subst): A
    def (a: A) free: Set[TName]
  }

  given Substitutable[Type] {

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

  given [A: Substitutable] : Substitutable[List[A]] {
    def (as: List[A]) `subFrom` (s: Subst) = as.map(_ `subFrom` s)
    def (as: List[A]) free                 = as.foldLeft(Set.empty)(_ ++ _.free)
  }

  given Substitutable[Scheme] {
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

  given [Env: TypeEnv] : Substitutable[Env] {
    def (e: Env) `subFrom` (s: Subst) = e.map(_ `subFrom` s)
    def (a: Env) free                 = a.elems.free
  }
}


type State[S, A] = S => (A, S)

trait Functor[F[_]] {
  def [A, B](fa: F[A]) map(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def [A, B](fa: F[A]) flatMap(f: A => F[B]): F[B]
}

trait StateF[S] extends Functor[[A] =>> State[S,A]] {
  def [A, B](sa: State[S,A]) map(f: A => B): State[S,B] = { state =>
    val (a, s) = sa(state)
    (f(a), s)
  }
}

trait StateM[S] extends StateF[S] with Monad[[A] =>> State[S,A]] {

  def pure[A](a: A): State[S,A] = state => (a, state)

  def [A, B](sa: State[S,A]) flatMap(f: A => State[S,B]): State[S,B] =
    { state =>
      val (a, s1) = sa(state)
      f(a)(s1)
    }
}

given [S]: StateF[S]

val letters: LazyList[String] = {
  import language.implicitConversions
  val alpha = ('a' to 'z').to(LazyList).map(_.toString)
  val numeric = for
    n <- LazyList.from(1)
    x <- alpha
  yield s"$x$n"
  alpha #::: numeric
}

import Substitutions._
import Substitutions.given

def (s1: Subst) `compose` (s2: Subst) = (s2 ++ s1).view.mapValues(_ `subFrom` s1).toMap

/////// ----- Usage ----- /////////

import ast.Tree._

val I = Lam("x", Var("x"))
val K = Lam("x", Lam("y", Var("x")))
val S = Lam("f", Lam("g", Lam("x", App(App(Var("f"), Var("x")), App(Var("g"), Var("x"))))))

val Bind = Lam("f", Lam("x", Let("y", Var("x"), App(Var("f"), Var("y")))))

puts(I)
puts(K)
puts(S)

puts(Bind)
