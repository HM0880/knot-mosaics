"""Encode knot mosaics as a SAT formula.

To run:
    python3 encode.py <m> <n> <AMO_method>

  where
    m is the number of rows in the board,
    n is the number of columns in the board, and
    AMO_method is the "at most one" encoding method (optional; default
    is pairwise).

This file has utility functions (e.g., tup2var) and encodes the
constraints (e.g., constraints for allowed tiles on the edges) to
encode a suitably connected knot.

Hannah Miller
https://hm0880.github.io/
September 2022

"""

import datetime
import sys

################################################################


class MakeBoard:
    """Board size m x n (rows x columns) and the number of tiles t."""

    def __init__(self, m_input, n_input, t_input=11):
        """Initialize the `board' object.  The SAT formula is included as
        part of the board object.

        For the square, unoriented version, there will always be
        exactly t=11 tiles, which is the default value of t_input.

        Per the PySAT documentation [1] and source code [2], we will write
        the SAT formula as a list of lists where each list contains
        integers.

        [1] https://pysathq.github.io/docs/html/api/solvers.html#pysat.solvers.Solver.add_clause
        [2] https://github.com/pysathq/pysat/blob/master/pysat/solvers.py#L967-L998

        """
        self.m = int(m_input)
        self.n = int(n_input)
        self.t = int(t_input)
        self.formula = []

    def get_largest_variable(self):
        """Return the largest variable (by absolute value) in the formula."""
        largest_var = 0  # initialize
        for clause in self.formula:
            for entry in clause:
                if abs(entry) > largest_var:
                    largest_var = abs(entry)
        return largest_var

    def prepend_dimacs_file_header_info(self, AMO_method):
        """Prepend DIMACS file header info to the formula.
        Call `prepend_dimacs_file_header_info' as the last step in the
        `encode_basic_constraints_for_suitably_connected_knot' function.

        """
        info = []
        num_vars = self.get_largest_variable()
        num_clauses = len(self.formula)
        info.append(f"c Board dimensions (rows x columns), {self.m} x {self.n}")
        info.append(f"c AT MOST ONE method, {AMO_method}")
        info.append(f"c Formula written on {datetime.datetime.now()}")
        info.append(f"p cnf {num_vars} {num_clauses}")
        self.formula = info + self.formula  # prepend info

    def print_formula(self):
        """Print the formula (which is a list of lists of integers) line
        by line as a string output.  This is used for writing the formula
        to its own text file.

        """
        for line in self.formula:
            if isinstance(line, list):
                print(" ".join([str(_) for _ in line]))
            else:
                print(line)  # print the header info as-is


################################################################
# Make a lookup list of each tile's allowed neighbors to have a
# suitably connected mosaic.  Tiles 7, 8, 9, and 10 have the same
# allowed neighbors.
tiles = [
    # left             right             above             below               tile number
    [
        [],
        [],
        [],
        [],
    ],
    [[2, 3, 5, 7, 8, 9, 10], [], [], [3, 4, 6, 7, 8, 9, 10]],  # tile 1
    [[], [1, 4, 5, 7, 8, 9, 10], [], [3, 4, 6, 7, 8, 9, 10]],  # tile 2
    [[], [1, 4, 5, 7, 8, 9, 10], [1, 2, 6, 7, 8, 9, 10], []],  # tile 3
    [[2, 3, 5, 7, 8, 9, 10], [], [1, 2, 6, 7, 8, 9, 10], []],  # tile 4
    [[2, 3, 5, 7, 8, 9, 10], [1, 4, 5, 7, 8, 9, 10], [], []],  # tile 5
    [[], [], [1, 2, 6, 7, 8, 9, 10], [3, 4, 6, 7, 8, 9, 10]],  # tile 6
    [
        [2, 3, 5, 7, 8, 9, 10],
        [1, 4, 5, 7, 8, 9, 10],
        [1, 2, 6, 7, 8, 9, 10],
        [3, 4, 6, 7, 8, 9, 10],
    ],  # tile 7
    [
        [2, 3, 5, 7, 8, 9, 10],
        [1, 4, 5, 7, 8, 9, 10],
        [1, 2, 6, 7, 8, 9, 10],
        [3, 4, 6, 7, 8, 9, 10],
    ],  # tile 8
    [
        [2, 3, 5, 7, 8, 9, 10],
        [1, 4, 5, 7, 8, 9, 10],
        [1, 2, 6, 7, 8, 9, 10],
        [3, 4, 6, 7, 8, 9, 10],
    ],  # tile 9
    [
        [2, 3, 5, 7, 8, 9, 10],
        [1, 4, 5, 7, 8, 9, 10],
        [1, 2, 6, 7, 8, 9, 10],
        [3, 4, 6, 7, 8, 9, 10],
    ],  # tile 10
]


def get_neighbors(k, direction):
    """Given a tile k and a direction (left, right, above, or below)
    as a string, get the neighbors of k.

    """
    if direction == "left":
        idx = 0
    elif direction == "right":
        idx = 1
    elif direction == "above":
        idx = 2
    elif direction == "below":
        idx = 3
    else:
        raise ValueError(f"direction `{direction}` not known")
    return tiles[k][idx]


################################################################
# Code to convert between tuples and variables


def in_bounds(board, i, j):
    """Given the board and a board location (i,j), return True if the
    location is within the m x n board bounds.  This handles the
    (literal) edge cases, which are most relevant for a tile's
    neighbors.

    """
    i_row = 0 <= i < board.m
    j_col = 0 <= j < board.n
    return i_row and j_col


def tup2var(board, i, j, k):
    """Given the board and an (i,j,k) tuple, return the variable v.

    Given the board, a board location (i,j), and a tile k, if (i,j)
    is within the board bounds, then return the SAT variable v, where
    v = (t)(in + j) + k + 1.  The `board` object has the values of t
    and n (m is not needed).  As the final step, add 1 to convert from
    a 0-indexed input to a 1-indexed output.

    The board position is 0-indexed; i.e., the upper left corner is the
    position (i,j) = (0,0).

    From the variable v, we can get i and j, so we can also easily get
    (i ± 1, j ± 1) when needed.

    """
    if in_bounds(board, i, j):
        return board.t * (i * board.n + j) + k + 1


def var2tup(board, v):
    """Given the board and a variable v, return the (i,j,k) tuple.

    As the first step, subtract 1 to convert from a 1-indexed input to a
    0-indexed output.  Also take the absolute value of v to handle
    conversion cases from the SAT solution where v was set to False
    (i.e., v is negative).

    The board position is 0-indexed; i.e., the upper left corner is the
    position (i,j) = (0,0).

    """
    v = abs(v) - 1
    k = v % board.t
    w = (v - k) / board.t
    j = w % board.n
    i = (w - j) / board.n
    return int(i), int(j), int(k)


################################################################
# Code for possible neighbors


def compute_neighbor_coordinate(board, v, direction):
    """Given the board, a variable v, and a direction (left, right,
    above, or below), return the board coordinates (a,b) of a neighbor
    tile in that direction.

    """
    i, j, _ = var2tup(board, v)
    if direction == "left":
        a = i
        b = j - 1
    elif direction == "right":
        a = i
        b = j + 1
    elif direction == "above":
        a = i - 1
        b = j
    elif direction == "below":
        a = i + 1
        b = j
    else:
        raise ValueError(f"direction `{direction}` not known")
    return a, b


def neighbors_in_one_direction(board, v, direction):
    """Given a variable v and a direction (left, right, above, or
    below), print a single clause of the possible neighbors of tile k
    (encoded by v) in that direction.

    The first variable in the clause is a negated v (i.e., -1*v),
    and the last entry is a clause-terminating `0`.

    """
    _, _, k = var2tup(board, v)
    a, b = compute_neighbor_coordinate(board, v, direction)
    if in_bounds(board, a, b):
        neighbors = [tup2var(board, a, b, ell) for ell in get_neighbors(k, direction)]
        if len(neighbors) > 0:
            board.formula.append([-1 * v] + neighbors + [0])


def all_neighbors(board, v):
    """Given a variable v, encode the allowed neighbors of tile k
    (encoded by v).  Print each direction on one line, which naturally
    gives a formula in CNF.

    """
    neighbors_in_one_direction(board, v, "left")
    neighbors_in_one_direction(board, v, "right")
    neighbors_in_one_direction(board, v, "above")
    neighbors_in_one_direction(board, v, "below")


################################################################
# Manage corners and edges


def one_corner_tile(board, i, j, nonzero_tile):
    """Given a coordinate (i,j) of a corner and the non-zero tile
    number that is allowed in that corner, return a clause of length 2
    which encodes the empty tile and the non-zero tile at those (i,j)
    coordinates.  Recall that a clause ends in 0.

    """
    empty = tup2var(board, i, j, 0)  # empty tile (tile 0)
    nonzero = tup2var(board, i, j, nonzero_tile)  # nonzero tile
    board.formula.append([empty, nonzero, 0])  # clause ending in 0


def one_edge_tile(board, i, j, a, b, c):
    """Given a coordinate (i,j) of an edge and three non-zero tiles
    a, b, and c, encode the constraints for that tile.

    """
    empty = tup2var(board, i, j, 0)
    tile_a = tup2var(board, i, j, a)
    tile_b = tup2var(board, i, j, b)
    tile_c = tup2var(board, i, j, c)
    board.formula.append([empty, tile_a, tile_b, tile_c, 0])


def tiles_and_allowed_corners(board):
    """Given the board, encode the tiles which are allowed in the
    corners.  There are only four clauses of length 2, so just do
    this by hand.

    """
    one_corner_tile(board, 0, 0, 2)  # T2; upper left
    one_corner_tile(board, 0, board.n - 1, 1)  # T1; upper right
    one_corner_tile(board, board.m - 1, 0, 3)  # T3; lower left
    one_corner_tile(board, board.m - 1, board.n - 1, 4)  # T4; lower right


def tiles_and_allowed_edges(board, edge_name, a, b, c):
    """Given an edge name (left, right, top, or bottom) and three
    non-zero tile numbers a, b, and c, encode the constraints for
    that edge.  Do not encode the corners; i.e., the index goes from
    [1,max-1).

    """
    if edge_name == "left":
        j = 0  # ith row; 0th column
        for i in range(1, board.m - 1):
            one_edge_tile(board, i, j, a, b, c)
    elif edge_name == "right":
        j = board.n - 1  # ith row; (n-1)th column
        for i in range(1, board.m - 1):
            one_edge_tile(board, i, j, a, b, c)
    elif edge_name == "top":
        i = 0  # 0th row; jth column
        for j in range(1, board.n - 1):
            one_edge_tile(board, i, j, a, b, c)
    elif edge_name == "bottom":
        i = board.m - 1  # (m-1)th row; jth column
        for j in range(1, board.n - 1):
            one_edge_tile(board, i, j, a, b, c)
    else:
        raise ValueError(f"edge name `{edge_name}` not known")


################################################################
# Code for exactly one tile

# This section gives a `at_least_one' function for encoding the AT
# LEAST ONE tile constraing.

# This section also has functions for different methods of encoding
# the AT MOST ONE (AMO) constraint.
# Only one function is used in the formula function
# `encode_basic_constraints_for_suitably_connected_knot`; the default
# is the pairwise method.


def at_least_one(board, i, j):
    """Encode AT LEAST ONE TILE on the board at location (i,j).  Do this
    by OR'ing of all the literals, ending with a clause-terminating 0.

    """
    if in_bounds(board, i, j):
        board.formula.append([tup2var(board, i, j, k) for k in range(board.t)] + [0])


def pairwise_AMO(board, i, j):
    """Given the board and a single (i,j) location, encode the
    ``exactly one tile in this location'' constraint with the
    folklore/naive/pairwise method.

    Its complexity is O(n^2), where n is the number of possible
    tiles at an (i,j) board location, so n=t=11.  The pairwise
    method is decided immediately.

    """
    # The pairwise OR negations, ending with a clause-terminating 0
    for k in range(board.t - 1):
        for ell in range(k + 1, board.t, 1):
            lit1 = -1 * tup2var(board, i, j, k)
            lit2 = -1 * tup2var(board, i, j, ell)
            board.formula.append([lit1, lit2, 0])


def sinz_sequential_unary_counter_AMO(board, i, j, av):
    """Given the board, a single (i,j) location, and an additional
    variable av, encode the ``exactly one tile in this location''
    constraint with the sequential unary counter method of Sinz [1].

    Sinz uses k to mean ``that not more than k out of the n
    variables x+1,...,x_n are allowed to be true.''  With Sinz's
    notation, SEQ's complexity is O(nk) for the number of clauses
    and O(nk) for the number of additional variables.  SEQ is
    decided by unit propagation.

    For our knot mosaics, n is the number of possible tiles at an
    (i,j) board location, so n=t=11.  For the AT MOST ONE
    contraint, k is 1.  In our usage, Sinz's SEQ is O(n) for the
    number of clauses and O(n) for the number of variables.

    ----

    [1] Sinz, Carsten.  Towards an optimal CNF encoding of Boolean
    cardinality constraints.  International conference on principles
    and practice of constraint programming. 2005.

    """
    # First clause; -x_1 OR s_1
    x1 = tup2var(board, i, j, 0)  # tile 0
    s1 = av
    board.formula.append([-1 * x1, s1, 0])

    # Loop; there are three pairwise clauses per tile
    av = av + 1  # increment
    for tile in range(
        1, board.t - 1
    ):  # 0 < tile < t (i.e., start at Tile 1 and do not include Tile 10)
        xi = tup2var(board, i, j, tile)  # variable x_i
        si = av  # additional variable s_i
        sh = av - 1  # additional variable s_{i-1}; use h for subscript

        board.formula.append([-1 * xi, si, 0])
        board.formula.append([-1 * sh, si, 0])
        board.formula.append([-1 * xi, -1 * sh, 0])

        av = av + 1  # increment

    # Last clause; -x_n OR -s_{n-1}
    xn = tup2var(
        board, i, j, board.t - 1
    )  # last tile t (need to adjust 11 -> 10 for 0-indexing)
    snminus1 = av - 1  # need to subtract 1 from the current additional variable av
    board.formula.append([-1 * xn, -1 * snminus1, 0])

    return av  # already incremented for the next (i,j) location


################################################################
# Code to produce the entire formula


def encode_basic_constraints_for_suitably_connected_knot(board, AMO_method="pairwise"):
    """Given an m x n board, produce the entire formula (including a
    DIMACS header) for a suitably connected knot.  To encode additional
    constraints, write and use another function.

    Board height m (i.e., rows), board width n (i.e., columns; set
    equal to m for a square board), and the number of tiles t
    (always 11 for square tiles).

    """
    # Encode the allowed corners.
    tiles_and_allowed_corners(board)

    # Encode the allowed edges.
    tiles_and_allowed_edges(board, "left", 2, 3, 6)
    tiles_and_allowed_edges(board, "right", 1, 4, 6)
    tiles_and_allowed_edges(board, "top", 1, 2, 5)
    tiles_and_allowed_edges(board, "bottom", 3, 4, 5)

    # Encode the allowed neighbors for every variable v, i.e., every
    # (i,j) position with every possible k-th tile.  Start the range at
    # 1 since the functions expect variables which are 1-indexed.
    for v in range(1, board.m * board.n * board.t):
        all_neighbors(board, v)

    # Encode the AT LEAST ONE constraint for all (i,j) locations.
    for i in range(board.m):
        for j in range(board.n):
            at_least_one(board, i, j)

    # Encode the AT MOST ONE (AMO) constraint for all (i,j) locations.
    # Also manage additional variables used by some of the AMO constraints.
    av = board.m * board.n * board.t + 1  # additional variable av
    for i in range(board.m):
        for j in range(board.n):
            if AMO_method == "pairwise":
                pairwise_AMO(board, i, j)
            elif AMO_method == "sinz_seq":
                av = sinz_sequential_unary_counter_AMO(board, i, j, av)
            else:
                raise ValueError(f"AMO_method `{AMO_method}` not supported")

    # Finally, prepend the DIMACS file header info to the formula.
    board.prepend_dimacs_file_header_info(AMO_method)

    return board.formula


################################################################
# Call from the command line

if __name__ == "__main__":
    # Get command line arguments A
    A = sys.argv
    m = sys.argv[1]
    n = sys.argv[2]
    try:
        AMO_method = sys.argv[3]
    except:
        AMO_method = "pairwise"

    # Make the board and encode the formula
    board_input = MakeBoard(m, n)
    formula = encode_basic_constraints_for_suitably_connected_knot(
        board_input, AMO_method
    )

    # Print the formula to STDOUT
    board_input.print_formula()
