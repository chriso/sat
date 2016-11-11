package chris.sat.solvers

import chris.sat.Result

/**
  * Solve 2-SAT CNF formulas by calculating strongly connected
  * components of an implication graph.
  *
  * The algorithm has polynomial running time.
  */
object SCCSolver extends Solver {

  protected def solverImpl(input: Step): Result = {
    require(input.formula.isExactlyKSat(2), "formula is not exactly 2-SAT")

    ???
  }
}
