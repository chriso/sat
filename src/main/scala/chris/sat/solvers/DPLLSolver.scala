package chris.sat.solvers

import chris.sat._

/**
  * Solve a boolean formula using the DPLL algorithm.
  *
  * The algorithm has a worst-case running time of O(2^n)
  *
  * See: https://en.wikipedia.org/wiki/DPLL_algorithm
  */
object DPLLSolver {

  case class Step(formula: Formula, solution: Set[Int])

  def solve(formula: Formula): Result =
    findSolution(Step(formula, Set.empty))

  /**
    * Recursively simplify the formula until a solution or
    * contradiction is found.
    */
  private def findSolution(input: Step): Result =
    simplify(input) match {
      case Step(formula, _) if formula.containsEmptyClause =>
        Unsatisfiable
      case Step(formula, solution) if formula.isEmpty =>
        Satisfiable(solution.toSeq: _*)
      case step @ Step(formula, _) =>
        branchAt(step, chooseLiteral(formula))
    }

  /**
    * Simplify a formula via unit propagation and pure literal elimination.
    */
  private def simplify(step: Step) =
    propagate(step, step.formula.pureLiterals ++ step.formula.unitClauseLiterals)

  /**
    * Propagate a set of literals through the formula.
    */
  private def propagate(step: Step, literals: Set[Int]) =
    Step(step.formula.propagate(literals), step.solution ++ literals)

  /**
    * Choose a literal to branch on.
    */
  private def chooseLiteral(formula: Formula): Int =
    formula.clauses.toStream.filterNot(_.isEmpty).head.literals.head

  private def branchAt(step: Step, literal: Int) =
    branch(
      findSolution(propagate(step, Set(literal))),
      findSolution(propagate(step, Set(-literal))))

  private def branch(a: => Result, b: => Result) = {
    val result = a
    if (result != Unsatisfiable) result else b
  }
}
