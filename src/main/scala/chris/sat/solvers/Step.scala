package chris.sat.solvers

import chris.sat.Formula

/**
  * A partially solved formula.
  */
case class Step(formula: Formula, solution: Set[Int] = Set.empty) {

  /**
    * Propagate literals through the formula.
    *
    * @param literals Literals to propagate
    * @return Step
    */
  def propagate(literals: Set[Int]): Step =
    if (literals.isEmpty) this
    else copy(formula.propagate(literals), solution ++ literals)
}
