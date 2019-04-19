
import scala.language.implicitConversions

/*-------------------------------------*
 *---- Lambda Calculus Interpreter ----* 
 *-------------------------------------*/

/*-- Auxilliary definitions for assessment of this worksheet -- */

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

implied for Show[String] = identity

implied [A] given Show[A] for Show[List[A]] {
  def (xs: List[A]) show = xs.map(_.show).mkString("[", ",", "]")
}

import Show._

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

def pretty(term: Term) = {
  def inner(i: Int, term: Term): String = term match {
    case Variable(x) => x
    case Lambda(x, m) =>
      val t = inner(0, m)
      val l = s"\\$x. $t"
      if i != 0 then s"($l)" else l
    case Apply(n,m) =>
      val n1 = inner(1,n)
      val m1 = inner(2, m)
      val a  = s"$n1 $m1"
      if i == 2 then s"($a)" else a
  }
  inner(0, term)
}

implied for Show[Term] = pretty

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

def numeral[I <: Int] = ???

// puts(numeral[0])
// puts(numeral[1])
// puts(numeral[2])

/* -------------- Assignment 2 -------------- */

val variables: Stream[Var] = ???

// puts(List(0,1,25,26,27,100,3039).map(variables(_)))

def filterVariables(xs: Seq[Var], ys: Seq[Var]): Seq[Var] = ???

// puts(
//   filterVariables(
//     List("y","z","a1","a2"),
//     List("y","a1","a3")
//   ).toList)

def fresh(vs: Seq[Var]): Var = ???

// puts(fresh(List("a","b","x")))

def used(t: Term): List[Var] = ???

// val usedD = used(demo0)
// puts(usedD)
// puts(fresh(usedD))

/* -------------- Assignment 3 -------------- */

def rename(x: Var, y: Var, t: Term): Term = ???

// puts(rename("b","z",demo0))

def substitute(x: Var, n: Term, t: Term): Term = ???

// puts(substitute("b", numeral[0], demo0))

/* -------------- Assignment 4 -------------- */

def beta(t: Term): Stream[Term] = ???

// val demo1 = Apply(demo0, numeral[1])
// puts(demo1)
// puts(beta(demo1).toList)

def normalize(t: Term): Unit = ???

// normalize(Apply(numeral[2], numeral[2]))

/* ---------------------------- */

def aBeta(t: Term): Stream[Term] = ???

def aNormalize(t: Term): Unit = ???

// aNormalize(Apply(numeral[2], numeral[2]))

/* ---------------------------- */

/**Should reduce in more steps with normal order
 */
val example1 = ???

// normalize(example1)
// aNormalize(example1)

/**Should reduce in more steps with applicative order
 */
val example2 = ???

// normalize(example2)
// aNormalize(example2)

/*-------------------------*
 *------- PART B ----------*
 *-------------------------*/

/* -------------- Assignment 5 (PAM) -------------- */

type PState = Nothing

// implied for Show[PState] = ???

val state1: PState = ???

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

def pStep(s: PState): PState = ???

def pFinal(s: PState): PState = ???

def pReadback(s: PState): Term = ???

def pRun(t: Term): Unit = ???

// pRun(term1)
// pRun(term2)

/* -------------- Assignment 6 (KAM) -------------- */

type Environment = Nothing
type State = Nothing

// implied for Show[Environment] = ???
// implied for Show[State] = ???

val state2: State = ???

val state3: State = ???

val state4: State = ???

// puts(state2)
// puts(state3)
// puts(state4)

def start(t: Term): State = ???

def step(s: State): State = ???

def finalState(s: State) = ???

def eval(t: Term, e: Environment): Term = ???

def readback(s: State): Term = ???

def run(t: Term): Unit = ???

// run(term1)
// run(readback(state2))