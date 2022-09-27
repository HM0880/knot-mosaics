"""Get all solutions of a knot mosaic SAT formula with PySAT bindings [1,2].

To run:
    python3 solve.py <m> <n> <mode>

  where
    m is the number of rows in the board,
    n is the number of columns in the board, and
    mode is "c" (count) or "w" (write) the satisfying assignments

////////////////

For quick testing, I'd recommend only going up to m,n of at most 4.

An example of counting the number of solutions is

    $ python3 solve.py 4 4 c

    solver, Cadical       , solutions found, 2594, total time (seconds), 0:00:00.590696
    solver, Gluecard3     , solutions found, 2594, total time (seconds), 0:00:00.298003
    solver, Gluecard4     , solutions found, 2594, total time (seconds), 0:00:00.314723
    solver, Glucose3      , solutions found, 2594, total time (seconds), 0:00:00.294252
    solver, Glucose4      , solutions found, 2594, total time (seconds), 0:00:00.314457
    solver, Lingeling     , solutions found, 2594, total time (seconds), 0:00:04.645189
    solver, MapleChrono   , solutions found, 2594, total time (seconds), 0:00:00.628719
    [[etc.]]


An example of writing the satisfying assignment ouptut is

    $ python3 solve.py 2 1 w
    1 -2 -3 -4 -5 -6 -7 -8 -9 -10 -11 12 -13 -14 -15 -16 -17 -18 -19 -20 -21 -22

Hannah Miller
https://hm0880.github.io/
September 2022

[1] https://pysathq.github.io/
[2] https://github.com/pysathq/pysat

"""

################################################################

import datetime
import pysat.formula
import pysat.solvers
import sys

import encode as utils  # custom Python file to encode basic constraints

################################################################


def solve_ALLSAT(solver_name, solver, formula, mode="c"):
    """Given a solver name, a PySAT solver object [1], and the SAT
    formula itself, find all satisfying assignments of the formula.
    PySAT's `solve' routine returns True when the formula can be
    satisfied [2].

    The mode is either counting the number of satisfying assignments
    ("c"; default) or writing the satisfying assignments to the
    terminal ("w").

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
            idx += 1  # increment the counter

            # Write the model (i.e., satisfying assignment)
            if mode == "w":
                print(" ".join([str(_) for _ in model]))

        else:
            stop = datetime.datetime.now()
            if mode == "c":  # report the counted number of solutions
                print(
                    f"solver, {solver_name:14s}, solutions found, {idx:4d}, total time (seconds), {stop-start}"
                )


################################################################

# For an ALLSAT solve of a 4x4 board,
#   - MapleCM is the slowest solver at ~20 seconds
#   - Lingeling is the second slowest solver at ~5 seconds

list_of_solvers = [
    # solver name, PySAT solver object
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
# Call from the command line

if __name__ == "__main__":
    # Get command line arguments A
    A = sys.argv
    m = sys.argv[1]
    n = sys.argv[2]
    try:
        mode = sys.argv[3]
    except:
        mode = "c"  # default to "c" for counting

    # Make the board and encode the formula
    board = utils.MakeBoard(m, n)
    formula = utils.encode_basic_constraints_for_suitably_connected_knot(board)

    # Run per the requested mode
    if mode == "c":  # count satisfying assignments
        for name, solver in list_of_solvers:
            solve_ALLSAT(name, solver, formula, mode)
    elif mode == "w":  # write satisfying assignments with MiniSat
        solve_ALLSAT("minisat", pysat.solvers.Minisat22(), formula, mode)
    else:  # raise error for unknown mode
        raise ValueError(f"mode `{mode}` is not supported; see doc string for help")
