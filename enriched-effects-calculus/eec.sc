
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
  @annotation.tailrec
  def inner(z: A, ts: List[Tree]): A = ts match {
    case Nil => z
    case t::ts => t match {
      case Pair(a,b)            => inner(f(z,t), a::b::ts)
      case App(g,u)             => inner(f(z,t), g::u::ts)
      case Eval(g,u)            => inner(f(z,t), g::u::ts)
      case Lam(_,u)             => inner(f(z,t), u::ts)
      case Lin(_,u)             => inner(f(z,t), u::ts)
      case CaseExpr(e,x,l,y,r)  => inner(f(z,t), e::l::r::ts)
      case Bang(u)              => inner(f(z,t), t::ts)
      case WhyNot(u)            => inner(f(z,t), t::ts)
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
          val z2 = wrapIfComplex(z,v1)
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
          s"\\$x. $t1"::s1
        }::acc

      case Lin(x,t) =>
        { s => val t1::s1 = s
          s"^\\$x. $t1"::s1
        }::acc

      case CaseExpr(e,x,l,y,r) =>
        { s => val e1::l1::r1::s1 = s
          s"case $e1 of {Inl($x). $l1; Inr($y). $r1}"::s1
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

      case Op(ops)  => (ops.toString::_)::acc
      case Point    => ("()"::_)::acc
      case Var(n)   => (s"$n"::_)::acc
      case Lit(c)   => (c.render::_)::acc
    }
  }

  prog.foldLeft(List.empty[String])((a,f) => f(a)).head
}

implied for Show[Tree] = pretty

/////// ----- Types ----- /////////

enum Prim derives Eql {
  case PVoid, PPoint
}

import Prim._

opaque type TName = String

object TName {
  def apply(s: String): TName = s.asInstanceOf[TName]
}

enum Type derives Eql {
  case TArr(a: Type, b: Type)
  // case TLin(a: Type, b: Type)
  // case TSum(a: Type, b: Type)
  // case TTen(a: Type, b: Type)
  // case TBang(a: Type)
  // case TPair(a: Type, b: Type)
  case TBase(p: Prim)
  case TVar(n: TName)
}

// def (a: Type) `TArr` (b: Type) = Type.TArr(a,b)

import Type._

val TPoint = TBase(PPoint)
val TVoid  = TBase(PVoid)

enum Scheme derives Eql { case Forall(vs: List[TName], t: Type) }

import Scheme._

/////// ----- Substitutions ----- /////////

opaque type Id = Long

object Id {
  private inline def (id: Id) asScala: Long =
    id.asInstanceOf[Long]

  private inline def apply(l: Long): Id =
    l.asInstanceOf[Id]

  val initId = 0l

  def (id: Id) succ: Id = Id { id.asScala + 1l }
}

import Id._

opaque type TypeEnv = Map[Name, Scheme]

object TypeEnv {
  import scala.collection.immutable.ListMap

  private inline def (e: TypeEnv) asScala =
    e.asInstanceOf[Map[Name, Scheme]]

  private inline def apply(e: Map[Name, Scheme]): TypeEnv =
    e.asInstanceOf[TypeEnv]

  def empty: TypeEnv =
    TypeEnv { Map.empty }

  def (e: TypeEnv) elems: List[Scheme] =
    ListMap(e.asScala.toSeq.sortBy(_._1):_*).values.toList

  def (e: TypeEnv) map(f: Scheme => Scheme): TypeEnv =
    TypeEnv { e.asScala.mapValues(f) }

  def (e: TypeEnv) extend(p: (Name, Scheme)): TypeEnv =
    TypeEnv { e.asScala + p }

  def (e: TypeEnv) restrict(x: Name): TypeEnv =
    TypeEnv { e.asScala - x }
}

import TypeEnv._

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
    case t: TBase     => t
    case t @ TVar(a)  => s.getOrElse(a, t)
    case t1 `TArr` t2 => TArr(t1 `subFrom` s, t2 `subFrom` s)
  }

  def (a: Type) free: Set[TName] = a match {
    case _:TBase      => Set.empty
    case TVar(a)      => Set(a)
    case t1 `TArr` t2 => t1.free ++ t2.free
  }
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

implied for Substitutable[TypeEnv] {
  def (e: TypeEnv) `subFrom` (s: Subst) = e.map(_ `subFrom` s)
  def (a: TypeEnv) free                 = a.elems.free
}

val emptySubst: Subst = Map.empty

def (s1: Subst) `compose` (s2: Subst) = (s2 ++ s1) `mapValues` (_ `subFrom` s1)

/////// ----- Usage ----- /////////

val I = Lam("x",Var("x"))
val K = Lam("x", Lam("y", Var("x")))
val S = Lam("f",Lam("g", Lam("x", App(App(Var("f"), Var("x")), App(Var("g"), Var("x"))))))

val bind = Lam("f", Lam("x", Let("y", Var("x"), App(Var("f"), Var("x")))))

puts(I)
puts(K)
puts(S)

puts(bind)