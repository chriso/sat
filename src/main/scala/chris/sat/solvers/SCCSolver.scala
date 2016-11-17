package chris.sat.solvers

import chris.sat._

/**
  * Solve 2-SAT CNF formulas by calculating strongly connected
  * components of an implication graph.
  *
  * The algorithm has polynomial running time.
  */
object SCCSolver extends Solver {

  protected def solverImpl(input: Step): Result = {
    require(input.formula.isExactlyKSat(2), "formula is not exactly 2-SAT")

    val graph = adjacencyList(implications(input.formula))
    val scc = tarjanSCC(graph)
    val solution = input.solution ++ sccSolution(scc)

    if (!isValid(solution)) Unsatisfiable
    else Satisfiable(solution.toSeq: _*)
  }

  private def implications(formula: Formula) =
    formula.clauses flatMap { case Clause(a, b) => Seq(-a -> b, -b -> a) }

  private def adjacencyList(implications: Seq[(Int, Int)]) =
    implications.foldLeft(Map.empty[Int, Seq[Int]]) {
      case (vertices, (tail, head)) =>
        val edges = vertices.getOrElse(tail, Seq.empty)
        vertices.updated(tail, edges :+ head)
    }

  private def tarjanSCC(adjacencyList: Map[Int, Seq[Int]]): Seq[Set[Int]] = {
    ???
  }

  private def isValid(solution: Set[Int]) =
    solution.forall { literal => !solution.contains(-literal) }

  private def sccSolution(scc: Seq[Set[Int]]): Set[Int] =
    scc.zipWithIndex.flatMap {
      case (literals, i) if i % 2 == 0 => literals
      case (literals, _) => literals map (-_)
    }.toSet
}
