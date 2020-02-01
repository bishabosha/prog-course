
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

inline def fst[A, B, C](f: A => C): (A, B) => C = ???

inline def church[I <: Int, A](f: A => A)(z: A) = ???

inline def numeral[I <: Int] = ???

// puts(numeral[0])
// puts(numeral[1])
// puts(numeral[2])

/* -------------- Assignment 2 -------------- */

val variables: LazyList[Var] = ???

// puts(List(0,1,25,26,27,100,3039).map(variables(_)))

def (xs: Seq[Var]) `filterVariables` (ys: Seq[Var]) = ???

// puts(
//   (List("y","z","a1","a2") `filterVariables` List("y","a1","a3")).toList)

def (vs: Seq[Var]) fresh = ???

// puts(List("a","b","x").fresh)

def (t: Term) used = ???

// val usedD = demo0.used
// puts(usedD)
// puts(usedD.fresh)

/* -------------- Assignment 3 -------------- */

def (t: Term) `rename` (x: Var, y: Var): Term = ???

// puts(demo0 `rename` ("b","z"))

def (t: Term) `substitute` (x: Var, n: Term): Term = ???

// puts(demo0 `substitute` ("b", numeral[0]))

/* -------------- Assignment 4 -------------- */

def (t: Term) beta: Option[Term] = ???

// val demo1 = Apply(demo0, numeral[1])
// puts(demo1)
// puts(demo1.beta)

def normalize(t: Term): Unit = ???

// normalize(Apply(numeral[2], numeral[2]))

/* ---------------------------- */

def (t: Term) aBeta: Option[Term] = ???

def aNormalize(t: Term): Unit = ???

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

def pStart(t: Term): PState = ???

def (s: PState) step = ???

def (s: PState) finalState = ???

def (s: PState) toTerm: Term = ???

def pRun(t: Term): Unit = ???

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

def start(t: Term): State = ???

def (s: State) step: State = ???

def (s: State) finalState = ???

def (t: Term) eval( e: Environment): Term = ???

def (s: State) toTerm: Term = ???

def run(t: Term): Unit = ???

// run(term1)
// run(state2.toTerm)
