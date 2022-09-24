"""Get all solutions of a knot mosaic SAT formula with PySAT bindings [1,2].

To run:
  python3 solve.py <m> <n>

For quick testing, I'd recommend only going up to m,n of at most 4.
The output should look something like

    $ python3 solve.py 4 4

    solver, Cadical       , solutions found, 2594, total time (seconds), 0:00:00.590696
    solver, Gluecard3     , solutions found, 2594, total time (seconds), 0:00:00.298003
    solver, Gluecard4     , solutions found, 2594, total time (seconds), 0:00:00.314723
    solver, Glucose3      , solutions found, 2594, total time (seconds), 0:00:00.294252
    solver, Glucose4      , solutions found, 2594, total time (seconds), 0:00:00.314457
    solver, Lingeling     , solutions found, 2594, total time (seconds), 0:00:04.645189
    solver, MapleChrono   , solutions found, 2594, total time (seconds), 0:00:00.628719
    [[etc.]]

Hannah Miller
https://hm0880.github.io/
September 2022

2022-04-23 (started)

[1] https://pysathq.github.io/
[2] https://github.com/pysathq/pysat

"""

################################################################

import datetime
import pysat.formula
import pysat.solvers
import sys

import encode  # custom Python file to encode basic constraints

################################################################


def solve(solver_name, solver, formula):
    """Given a solver name, a PySAT solver object [1], and the SAT
    formula itself, find all satisfying assignments of the formula.

    PySAT's `solve' routine returns True when the formula can be
    satisfied [2].

    [1] https://pysathq.github.io/docs/html/api/solvers.html
    [2] https://pysathq.github.io/docs/html/api/solvers.html#pysat.solvers.Solver.solve

    """
    # Add the clauses to the chosen solver
    for line in formula:
        if isinstance(line, list):
            solver.add_clause(line[:-1])  # remove clause-terminating 0 for PySAT

    # Add blocking clauses to solve ALLSAT
    is_SAT = True  # initialize
    idx = 0
    start = datetime.datetime.now()
    while is_SAT:
        is_SAT = solver.solve()  # try to solve
        if is_SAT:
            model = solver.get_model()  # "model" = current satisfying assignment
            solver.add_clause([-1 * x for x in model])  # add blocking clause

            # print(model)
            variables_that_are_true = [str(x) for x in model if x > 0]
            # print(' '.join(variables_that_are_true))

            idx += 1  # increment
        else:
            stop = datetime.datetime.now()
            print(
                f"solver, {solver_name:14s}, solutions found, {idx:4d}, total time (seconds), {stop-start}"
            )


################################################################

# MapleCM slowest solver at ~20 seconds for full solve
# Lingeling is 2nd slowest solver at ~5 seconds for full solve

list_of_solvers = [
    # solver name    PySAT solver object
    ["Cadical", pysat.solvers.Cadical()],
    ["Gluecard3", pysat.solvers.Gluecard3()],
    ["Gluecard4", pysat.solvers.Gluecard4()],
    ["Glucose3", pysat.solvers.Glucose3()],
    ["Glucose4", pysat.solvers.Glucose4()],
    ["Lingeling", pysat.solvers.Lingeling(),],
    ["MapleChrono", pysat.solvers.MapleChrono()],
    ["MapleCM", pysat.solvers.MapleCM(),],
    ["Maplesat", pysat.solvers.Maplesat()],
    ["Mergesat3", pysat.solvers.Mergesat3()],
    ["Minicard", pysat.solvers.Minicard()],
    ["Minisat22", pysat.solvers.Minisat22()],
    ["MinisatGH", pysat.solvers.MinisatGH()],
]

################################################################

# Call from command line
board = encode.MakeBoard(sys.argv[1], sys.argv[2])
formula = encode.encode_basic_constraints_for_suitably_connected_knot(board)

# Try all solvers
print("")
for name, solver in list_of_solvers:
    solve(name, solver, formula)

# Run individual solvers
solve('minisat',pysat.solvers.Minisat22(),formula)
solve('glucose4',pysat.solvers.Glucose4(),formula)
