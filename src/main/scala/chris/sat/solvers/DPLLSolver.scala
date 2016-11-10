package chris.sat.solvers

import chris.sat._

/**
  * Solve a boolean formula using the DPLL algorithm.
  *
  * The algorithm has a worst-case running time of O(2^n)
  *
  * See: https://en.wikipedia.org/wiki/DPLL_algorithm
  */
object DPLLSolver extends Solver {

  def solve(formula: Formula): Result =
    findSolution(Step(formula))

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
    step
      .propagate(step.formula.unitClauseLiterals)
      .propagate(step.formula.pureLiterals)

  /**
    * Choose a literal to branch on. The formula is guaranteed to have at
    * least one non-empty clause.
    */
  private def chooseLiteral(formula: Formula): Int =
    formula.clauses.find(!_.isEmpty).get.literals.head

  private def branchAt(step: Step, literal: Int) =
    branch(
      findSolution(step.propagate(Set(literal))),
      findSolution(step.propagate(Set(-literal))))

  private def branch(a: => Result, b: => Result) = {
    val result = a
    if (result != Unsatisfiable) result else b
  }
}
