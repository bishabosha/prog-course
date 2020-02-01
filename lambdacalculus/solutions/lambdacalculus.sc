
import scala.language.implicitConversions

/*-------------------------------------*
 *---- Lambda Calculus Interpreter ----*
 *-------------------------------------*/

/*-- Auxilliary definitions for assessment of this worksheet -- */

object FuncLib {

  @FunctionalInterface
  trait Show[A] {
    /**Used for converting values to a String for printing
    */
    def (a: A) show: String
  }

  /**Displays a value given an instance of Show.*/
  def puts[A](a: A)(given Show[A]) = println(show(a))
  def show[A](a: A)(given Show[A]) = summon[Show[A]].show(a)

  object Show {

    given Show[String] = identity

    given [A](given Show[A]): Show[List[A]] =
      _.map(_.show).mkString("[", ",", "]")

    given [A](given Show[A]): Show[Option[A]] =
      _.toList.show

    given [A,B](given Show[A], Show[B]): Show[(A,B)] =
      (a,b) => s"(${a.show}, ${b.show})"

    given [A,B,C](given Show[A], Show[B], Show[C]): Show[(A,B,C)] =
      (a,b,c) => s"(${a.show}, ${b.show}, ${c.show})"

  }

}

import FuncLib._

/*-------------------------*
 *------- PART A ----------*
 *-------------------------*/

type Var = String

enum Term derives Eql {
  case Variable(x: Var)
  case Lambda(x: Var, t: Term)
  case Apply(m: Term, n: Term)
}

import Term._

def (term: Term) pretty = {
  def inner(i: Int, term: Term): String = term match {
    case Variable(x) => x

    case Lambda(x, m) =>
      val l = s"\\$x. ${inner(0, m)}"
      if i != 0 then s"($l)" else l

    case Apply(n,m) =>
      val a  = s"${inner(1,n)} ${inner(2, m)}"
      if i == 2 then s"($a)" else a
  }
  inner(0, term)
}

given Show[Term] = pretty

val demo0 =
  Lambda("a",
    Lambda("x",
      Apply(
        Apply(
          Lambda("y", Variable("a")),
          Variable("x")),
        Variable("b"))))

// puts(demo0)

/* -------------- Assignment 1 -------------- */

inline def fst[A, B, C](f: A => C): (A, B) => C =
  (a, _) => f(a)

inline def church[I <: Int, A](f: A => A)(z: A) = {
  (0 until valueOf[I]).foldLeft(z)(fst(f))
}

inline def numeral[I <: Int] = {
  val f = Variable("f")
  val x = Variable("x")
  val n = church[I, Term](Apply(f,_))(x)
  Lambda("f", Lambda("x", n))
}

// puts(numeral[0])
// puts(numeral[1])
// puts(numeral[2])

/* -------------- Assignment 2 -------------- */

val variables: LazyList[Var] = {
  val alpha = ('a' to 'z').to(LazyList).map(_.toString)
  val numeric = for
    n <- LazyList.from(1)
    x <- alpha
  yield s"$x$n"
  alpha #::: numeric
}

// puts(List(0,1,25,26,27,100,3039).map(variables(_)))

def (xs: Seq[Var]) `filterVariables` (ys: Seq[Var]) =
  xs.filterNot(ys.contains)

// puts(
//   (List("y","z","a1","a2") `filterVariables` List("y","a1","a3")).toList)

def (vs: Seq[Var]) fresh = (variables `filterVariables` vs).head

// puts(List("a","b","x").fresh)

def (t: Term) used = {
  def inner(acc: List[Var], ts: List[Term]): List[Var] = ts match {
    case Nil => acc
    case t :: ts => t match {
      case Variable(x)  => inner(x::acc, ts)
      case Apply(t, u)  => inner(acc, t::u::ts)
      case Lambda(x, t) => inner(x::acc, t::ts)
    }
  }
  inner(Nil, t :: Nil).sorted.distinct
}

// val usedD = demo0.used
// puts(usedD)
// puts(usedD.fresh)

/* -------------- Assignment 3 -------------- */

def (t: Term) `rename` (x: Var, y: Var): Term = t match {
  case Variable(z) => Variable(if z == x then y else z)
  case Lambda(z,n) => Lambda(z, if z == x then n else n `rename` (x,y))
  case Apply(n, m) => Apply(n `rename` (x,y), m `rename` (x,y))
}

// puts(demo0 `rename` ("b","z"))

def (t: Term) `substitute` (x: Var, n: Term): Term = t match {
  case v @ Variable(y) => if y == x then n else v

  case l @ Lambda(y, m) =>
    val z = fresh (x :: used(m) ::: used(n))
    if y == x then l
    else Lambda(z, (m `rename` (y,z)) `substitute` (x,n))

  case Apply(m1, m2) => Apply(m1 `substitute` (x,n), m2 `substitute` (x,n))
}

// puts(demo0 `substitute` ("b", numeral[0]))

/* -------------- Assignment 4 -------------- */

def (t: Term) beta: Option[Term] = t match {
  case Apply(Lambda(x,m),n) => Some(m `substitute` (x,n))

  case Apply(m,n) =>
    m.beta.map(Apply(_,n)) orElse n.beta.map(Apply(m,_))

  case Lambda(x,t)  => t.beta.map(Lambda(x,_))
  case Variable(_)  => Option.empty
}

// val demo1 = Apply(demo0, numeral[1])
// puts(demo1)
// puts(demo1.beta)

def normalize(t: Term): Unit = {
  puts(t)
  beta(t).fold(())(normalize)
}

// normalize(Apply(numeral[2], numeral[2]))

/* ---------------------------- */

def (t: Term) aBeta: Option[Term] = t match {
  case Apply(l @ Lambda(x, m), n) =>
    def fromL = l.aBeta.map(Apply(_, n))
    def fromN = n.aBeta.map(Apply(l, _))
    fromL orElse fromN orElse Some(m `substitute` (x,n))

  case Apply(m, n) =>
    def fromM = m.aBeta.map(Apply(_, n))
    def fromN = n.aBeta.map(Apply(m, _))
    fromM orElse fromN

  case Lambda(x, t) => t.aBeta.map(Lambda(x, _))
  case Variable(_)  => Option.empty
}

def aNormalize(t: Term): Unit = {
  puts(t)
  aBeta(t).headOption.fold(())(aNormalize)
}

// aNormalize(Apply(numeral[2], numeral[2]))

/* ---------------------------- */

/**Should reduce in more steps with normal order
 */
val example1 =
  Apply(
    Lambda("x",
      Apply(Variable("x"), Variable("x"))),
    Apply(
      Lambda("y", Variable("y")),
      Variable("z")))

// normalize(example1)
// aNormalize(example1)

/**Should reduce in more steps with applicative order
 */
val example2 =
  Apply(
    Apply(
      Lambda("x",
        Lambda("y", Variable("x"))),
      Variable("x")),
    Lambda("a",
      Apply(
        Lambda("b", Variable("c")),
        Variable("a"))))

// normalize(example2)
// aNormalize(example2)

/*-------------------------*
 *------- PART B ----------*
 *-------------------------*/

/* -------------- Assignment 5 (PAM) -------------- */

type PState = (Term, List[Term])

val state1 = (
  Lambda("x", Lambda("y", Variable("x"))),
  List(Variable("Yes"), Variable("No")))

val term1 =
  Apply(
    Apply(
      Lambda("x",
        Lambda("y",Variable("x"))),
      Variable("Yes")),
    Variable("No"))

val term2 =
  Apply(
    Apply(
      Lambda("b",
        Apply(demo0, Variable("Yes"))),
      Lambda("z", Variable("z"))),
    Variable("No"))

// puts(state1)
// puts(term1)
// puts(term2)

def pStart(t: Term): PState = (t, Nil)

def (s: PState) step = (s: @unchecked) match {
  case (Lambda(x,m), s::ss) => (m `substitute` (x,s), ss)
  case (Apply(n,m), ss)     => (n, m::ss)
}

def (s: PState) finalState = s match {
  case (_: Lambda , Nil) => true
  case (_: Variable, _)  => true
  case _                 => false
}

def (s: PState) toTerm: Term = s match {
  case (t, Nil)   => t
  case (t, s::ss) => toTerm(Apply(t,s), ss)
}

def pRun(t: Term): Unit = {
  def loop(s: PState): Unit = {
    puts(s)
    if s.finalState then puts(s.toTerm)
    else loop(s.step)
  }
  loop(pStart(t))
}

// pRun(term1)
// pRun(term2)

/* -------------- Assignment 6 (KAM) -------------- */

enum Environment {
  case Env(x: Var, t: Term, e: Environment, es: Environment)
  case Z
}

import Environment._

type EnvList = List[(Var, Term, Environment)]

def (ep: Environment) toList(acc: EnvList): EnvList = ep match {
  case Z             => acc
  case Env(x,n,e,es) => es.toList((x,n,e)::acc)
}

given Show[Environment] = env =>
  FuncLib.Show.given_Show_List[(Var, Term, Environment)].show(env.toList(Nil))

type State = (Term, Environment, List[(Term, Environment)])

val state2: State =
  (
    Apply(Lambda("x", Variable("x")),
      Variable("y")),
    Env("y", Lambda("z", Variable("z")), Z, Z),
    List.empty
  )

val state3: State  =
  (
    Apply(Variable("x"), Variable("x")),
    Env(
      "x",
      Lambda("x", Apply(Variable("x"), Variable("x"))),
      Z,
      Z),
    List.empty
  )

val state4: State =
  (
    Lambda("y", Variable("x")),
    Z,
    List((
      Variable("z"),
      Env(
        "z",
        Lambda("a", Variable("b")),
        Env(
          "b",
          Variable("c"),
          Z,
          Z),
        Z)
    ))
  )

// puts(state2)
// puts(state3)
// puts(state4)

def start(t: Term): State = (t, Z, Nil)

def (s: State) step: State = (s: @unchecked) match {
  case (v @ Variable(x), Env(z,n,f,e), s) =>
    if x == z then (n, f, s)
    else (v, e, s)

  case (Lambda(x,n), e, (m,f)::s) => (n, Env(x, m, f, e), s)
  case (Apply(n,m), e, s)         => (n, e, (m,e)::s)
}

def (s: State) finalState = s match {
  case (_: Lambda, _, Nil) => true
  case (_: Variable, Z, _) => true
  case _                   => false
}

def (t: Term) eval( e: Environment): Term = (t, e) match {
  case (v @ Variable(x), Env(y,n,e,es)) =>
    if x == y then n.eval(e) else v.eval(es)

  case (v: Variable, Z) => v
  case (Lambda(x,n), e) => Lambda(x, n.eval(Env(x, Variable(x), Z, e)))
  case (Apply(n,m), e)  => Apply(n.eval(e), m.eval(e))
}

def (s: State) toTerm: Term = s match {
  case (n, e, Nil)     => n.eval(e)
  case (n,_,(m,e)::cs) => toTerm(Apply(n.eval(e), m.eval(e)), e, cs)
}

def run(t: Term): Unit = {
  def loop(s: State): Unit = {
    puts(s)
    if s.finalState then puts(s.toTerm)
    else loop(step(s))
  }
  loop(start(t))
}

// run(term1)
// run(state2.toTerm)
