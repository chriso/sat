package chris.sat.solvers

import chris.sat._

import org.scalatest.{FlatSpec, Matchers}

class DPLLSolver$Test extends FlatSpec with Matchers {

  behavior of "DPLLSolver$"

  private def sat(formula: Formula) = {
    DPLLSolver.solve(formula) should matchPattern {
      case Satisfiable(literals @ _*)
        if formula.isSatisfiedBy(literals.toSet) =>
    }
  }

  private def unsat(formula: Formula) = {
    DPLLSolver.solve(formula) should matchPattern { case Unsatisfiable => }
  }

  it should "solve satisfiable CNF formulas" in {
    sat(Formula(Clause(1), Clause(2), Clause(3, 4)))
    sat(Formula(Clause(1), Clause(-1, 2), Clause(-2, 3)))
    sat(Formula(Clause(1, -2, 3, 4), Clause(-1, -2, -3, 4), Clause(1, 2, 3, -4)))
    sat(Formula())
  }

  it should "solve unsatisfiable CNF formulas" in {
    unsat(Formula(Clause()))
    unsat(Formula(Clause(1), Clause(-1)))
    unsat(Formula(Clause(1, 2), Clause(-1), Clause(-2)))
  }
}
