Adventures in [SAT](https://en.wikipedia.org/wiki/Boolean_satisfiability_problem).

I was interested in seeing how 2-SAT can be solved in polynomial time, and then understanding why this approach doesn't translate to 3-SAT. Answer: in 2-SAT, you can build an [implication graph](https://en.wikipedia.org/wiki/Implication_graph) and then check strongly-connected components for unsatisfiable contradictions.
