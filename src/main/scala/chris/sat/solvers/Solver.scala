package chris.sat.solvers

import chris.sat.{Formula, Result}

/**
  * Base trait for solvers.
  */
trait Solver {

  /**
    * Solve a formula.
    *
    * @param formula Formula to solve
    * @return Result
    */
  def solve(formula: Formula): Result
}
