;;;; Encode mosaic knots for a SAT solver.
;;;;
;;;; This file has utility functions (e.g., tup2var) and encodes the constraints
;;;; (e.g., constraints for allowed tiles on the edges) to encode a suitably
;;;; connected knot.  Additional constraints (e.g., requiring a specific tile at
;;;; a specific location) will probably be done in other files (e.g., the
;;;; `make-sat.lisp` file).
;;;;
;;;; 2021-04-04 (started)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define m,n,t here; use `defparameter` so that we can change the
;; values at the end of this file.
;; Board height *m* (i.e., rows), board width *n* (i.e., columns; set
;; equal to *m* for a square board), and the number of tiles *t*
;; (always 11 for square tiles)
(defparameter *m* 4)
(defparameter *n* 4)
(defparameter *t* 11)

(defvar *neighbors-table* "Hash table for all 10 non-blank tiles")
(setq *neighbors-table* (make-hash-table))

(defun add-neighbors (k left-right-above-below)
  "Given a tile k and a list of left, right, above, and below allowed neighbors,
set the list in the hash table."
  (setf (gethash k *neighbors-table*) left-right-above-below))

(defun get-neighbors (k dir)
  "Given a tile k and a direction dir, return the list of allowed neighbors in
that direction.  This is a helper function so we don't have to remember that the
first element in the list is the left neighbors, the second element is the right
neighbors, etc."
  (cond ((string-equal dir "left")
         (first (gethash k *neighbors-table*)))
        ((string-equal dir "right")
         (second (gethash k *neighbors-table*)))
        ((string-equal dir "above")
         (third (gethash k *neighbors-table*)))
        ((string-equal dir "below")
         (fourth (gethash k *neighbors-table*)))))

(defvar *7-8-9-10-allowed-neighbors*
  '((2 3 5 7 8 9 10) (1 4 5 7 8 9 10) (1 2 6 7 8 9 10) (3 4 6 7 8 9 10))
  "Tiles 7, 8, 9, and 10 have the same allowed neighbors; define them here.")

;; Add each tile's allowed neighbors
;;                  left             right            above            below
(add-neighbors  1 '((2 3 5 7 8 9 10) ()               ()               (3 4 6 7 8 9 10)))
(add-neighbors  2 '(()               (1 4 5 7 8 9 10) ()               (3 4 6 7 8 9 10)))
(add-neighbors  3 '(()               (1 4 5 7 8 9 10) (1 2 6 7 8 9 10) ()))
(add-neighbors  4 '((2 3 5 7 8 9 10) ()               (1 2 6 7 8 9 10) ()))
(add-neighbors  5 '((2 3 5 7 8 9 10) (1 4 5 7 8 9 10) ()               ()))
(add-neighbors  6 '(()               ()               (1 2 6 7 8 9 10) (3 4 6 7 8 9 10)))
(add-neighbors  7 *7-8-9-10-allowed-neighbors*)
(add-neighbors  8 *7-8-9-10-allowed-neighbors*)
(add-neighbors  9 *7-8-9-10-allowed-neighbors*)
(add-neighbors 10 *7-8-9-10-allowed-neighbors*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun in-bounds (i j)
  "Given a board location (i,j), return T if the location is within the *m* x *n*
board bounds.  This handles the (literal) edge cases, which are most relevant for
a tile's neighbors."
  (and (>= i 0) (< i *m*)
       (>= j 0) (< j *n*)))

(defun tup2var (i j k)
  "Given a board location (i,j) and a tile k, when (i,j) is within the board
bounds, then compute the index v, where v = (t)(in + j) + k + 1.  The `+1` is
the last step to convert from a 0-indexed input to a 1-indexed output."
  (when (in-bounds i j)
    (+ (+ (* *t* (+ (* i *n*) j)) k) 1)))

(defun var2tup (v_input)
  "Convert a variable v to its equivalent (i,j,k) tuple.  The `-1` is the first
step to convert from a 1-indexed input to a 0-indexed output.  Also take the
absolute value of v."
  (let* ((v (- (abs v_input) 1))
         (k (mod v *t*))
         (w (/ (- v k) *t*))
         (j (mod w *n*))
         (i (/ (- w j) *n*)))
    (list i j k)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun exactly-one-tile (i j)
  "Given a single (i,j) location, encode the 'exactly one tile in this location'
constraint.  Another function will loop through all the (i,j) locations and call
exactly-one-tile for each location."
  ;; The OR'ing of all the literals, ending with a clause-terminating 0
  (when (in-bounds i j)
    (loop for k from 0 to (- *t* 1) do
      (let ((v (tup2var i j k)))
        (format t "~a " v))
          finally (format t "~a~%" 0)))
  ;; The pairwise OR negations, ending with a clause-terminating 0
  (loop for k from 0 to (- *t* 2) do
    (loop for ell from (+ k 1) to (- *t* 1) do
      (let ((v1 (tup2var i j k))
            (v2 (tup2var i j ell)))
        (format t "-~d -~d 0~%" v1 v2)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-neighbor-coordinate (v dir)
  "Given a variable v and a direction, return the neighbor's board coordinates."
  (let* ((i (first (var2tup v)))
         (j (second (var2tup v))))
    (cond ((string-equal dir "left")
           (list i (- j 1)))
          ((string-equal dir "right")
           (list i (+ j 1)))
          ((string-equal dir "above")
           (list (- i 1) j))
          ((string-equal dir "below")
           (list (+ i 1) j)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun return-neighbors-in-one-direction (v dir)
  "Given variable v and direction dir, print a single clause of the possible
neighbors of k in that direction."
  (let* ((delta-i (first  (compute-neighbor-coordinate v dir)))
         (delta-j (second (compute-neighbor-coordinate v dir)))
         (k (third (var2tup v))))
    (when (in-bounds delta-i delta-j)
      (loop for a in (get-neighbors k dir)
            collect (tup2var delta-i delta-j a) into neighbors-of-v
            finally (return neighbors-of-v)))))

(defun single-direction-print-neighbors (v dir)
  "If neighbors do exist, then print the variable v and v's neighbors."
  (let ((neighbors-of-v (return-neighbors-in-one-direction v dir)))
    (when (< 0 (length neighbors-of-v))
      (format t "-~d " v)
      (format t "~{~a~^ ~}" neighbors-of-v)
      (format t " ~a~%" 0))))

(defun print-all-neighbors (v)
  "Given variable v, encode the allowed neighbors of k.
Print each direction on one line, which naturally gives a formula in CNF."
  (single-direction-print-neighbors v "left")
  (single-direction-print-neighbors v "right")
  (single-direction-print-neighbors v "above")
  (single-direction-print-neighbors v "below"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun one-corner-tile (x y non-zero-tile)
  "Given a coordinate (x,y) of a corner and the non-zero tile number that is
allowed in that corner, return a clause of length 2 which encodes the empty tile
and the non-zero tile at those (x,y) coordinates.  Recall: a clause ends in 0."
  (format t "~d ~d 0~%"  ;; the clause ending in 0
          (tup2var x y 0)  ;; the empty tile (tile 0)
          (tup2var x y non-zero-tile)))  ;; the non-zero tile

(defun one-edge-tile (x y a b c)
  "Given a coordinate (x,y) of an edge and three non-zero tiles a, b, and c,
encode the constraints for that tile."
  (format t "~d ~d ~d ~d 0~%"
          (tup2var x y 0)
          (tup2var x y a)
          (tup2var x y b)
          (tup2var x y c)))

;;;;;;;;;;;;;;;;

(defun tiles-and-allowed-corners ()
  "Encode the tiles which are allowed in the corners. There are only four
clauses of length 2, so just do this by hand."
  (let* ((mm (- *m* 1))  ;; largest row
         (nn (- *n* 1)))  ;; largest column
    (one-corner-tile  0  0 2)  ;; upper left
    (one-corner-tile  0 nn 1)  ;; upper right
    (one-corner-tile mm  0 3)  ;; lower left
    (one-corner-tile mm nn 4)))  ;; lower right

(defun tiles-and-allowed-edges (edge-text a b c)
  "Given an edge text and three non-zero tile numbers a, b, and c, encode the
 constraints for that edge.  Do *not* encode the corners; i.e., the index goes
 from [1,max-1)."
  (cond ((string-equal edge-text "left")
         (loop for i from 1 to (- *m* 2) do
           (one-edge-tile i 0 a b c)))  ;; x = ith row; y = 0th column
        ((string-equal edge-text "right")
         (loop for i from 1 to (- *m* 2) do
           (one-edge-tile i (- *n* 1) a b c)))  ;; x = ith row; y = (n-1)th column
        ((string-equal edge-text "top")
         (loop for j from 1 to (- *n* 2) do
           (one-edge-tile 0 j a b c)))  ;; x = 0th row; y = jth column
        ((string-equal edge-text "bottom")
         (loop for j from 1 to (- *n* 2) do
           (one-edge-tile (- *m* 1) j a b c)))))  ;; x = (m-1)th row; y = jth column


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun require-at-least-1-tile-of-this-type (tile-type)
  "Encode all (i,j) pairs for TILE-TYPE.  Print as one line with a trailing 0."
  (loop for i from 0 to (- *m* 1) do
    (loop for j from 0 to (- *n* 1) do
      (format t "~a " (tup2var i j tile-type))))
  (format t "~a~%" 0))

(defun require-at-least-3-crossing-tiles ()
  (loop for i1 from 0 to (- *m* 1) do
    (loop for j1 from 0 to (- *n* 1) do
      (loop for i2 from 0 to (- *m* 1) do
        (loop for j2 from 0 to (- *n* 1) do
          (when (not
                 (and (eq i1 i2) (eq j1 j2)))  ;; (i1,j1) == (i2,j2)
            ;; Format the negations of the T9 and T10 variables
            (format t "-~a " (tup2var i1 j1  9))
            (format t "-~a " (tup2var i2 j2 10))
            (loop for i3 from 0 to (- *m* 1) do
              (loop for j3 from 0 to (- *n* 1) do
                (when (not
                       (or
                        (and (eq i1 i3) (eq j1 j3))  ;; (i1,j1) == (i3,j3)
                        (and (eq i2 i3) (eq j2 j3))))  ;; (i2,j2) == (i3,j3)
                  ;; Format the large OR portion
                  (format t "~a ~a " (tup2var i3 j3 9) (tup2var i3 j3 10)))))
            ;; Print the trailing zero
            (format t "~a~%" 0)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun one-clocking (i j var-list)
  "Given a location (i,j) and a list of variables, encode the banning of the
four tiles in the orientation

  b a
  c d

which is the ordering that we will call

  loc2 loc1
  loc3 loc4
"
  (format t "-~a -~a -~a -~a 0~%"
          (tup2var    i    (+ j 1) (first  var-list))    ;; same row, +1 column
          (tup2var    i       j    (second var-list))    ;; use the input coordinate
          (tup2var (+ i 1)    j    (third  var-list))    ;; +1 row, same column
          (tup2var (+ i 1) (+ j 1) (fourth var-list))))  ;; +1 row, +1 column

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-mapping (clocking var-list)
  "Given a clocking direction (+90, +180, or +270) and a list of variables,
return a new list of those variables rotated.  The mapping is always
counterclockwise (CCW).

Two things must happen per tile: (1) The tile must be mapped to the new tile
name, and (2) the tile must be placed in the appropriate order in the output
list since `one-clocking' encodes the mosaic board location based on the input
order of its list.

Recall that `one-clocking' encodes a,b,c,d as

    b a
    c d
"

  (let
      ;; Define lists, which we will access with the `elt' function
      ;;             0   1 2 3 4   5 6   7 8    9 10
      (( *+90* (list 0   2 3 4 1   6 5   8 7   10  9))
       (*+180* (list 0   3 4 1 2   5 6   7 8    9 10))
       (*+270* (list 0   4 1 2 3   6 5   8 7   10  9)))

    (cond ((string-equal clocking "+90")
           (list (elt  *+90* (fourth var-list))    ;; d in loc1
                 (elt  *+90* (first  var-list))    ;; a in loc2
                 (elt  *+90* (second var-list))    ;; b in loc3
                 (elt  *+90* (third  var-list))))  ;; c in loc4
          ((string-equal clocking "+180")
           (list (elt *+180* (third  var-list))    ;; c in loc1
                 (elt *+180* (fourth var-list))    ;; d in loc2
                 (elt *+180* (first  var-list))    ;; a in loc3
                 (elt *+180* (second var-list))))  ;; b in loc4
          ((string-equal clocking "+270")
           (list (elt *+270* (second var-list))    ;; b in loc1
                 (elt *+270* (third  var-list))    ;; c in loc2
                 (elt *+270* (fourth var-list))    ;; d in loc3
                 (elt *+270* (first  var-list))))  ;; a in loc4
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun 1-coord-4-clockings (i j var-list)  ;;v1 v2 v3 v4)
  "We have (v1 AND v2 AND v3) => NOT v4
       NOT (v1 AND v2 AND v3) OR NOT v4
 (NOT v1 OR NOT v1 OR NOT v3) OR NOT v4

where v1,v2,v3,v4 are the input tiles.  Use `vN' to denote the tiles to not
confuse with referring to the N-th tile `tN'.

Tile order is counterclockwise (CCW) from the upper right of the 4-tile
quadrant:

  loc2 loc1
  loc3 loc4

The input (i,j) coordinates are based on the `loc2' position.
"
  (one-clocking i j                     var-list)  ;; input list
  (one-clocking i j (get-mapping  "+90" var-list))
  (one-clocking i j (get-mapping "+180" var-list))
  (one-clocking i j (get-mapping "+270" var-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun all-coords-ban-2x2-configs (a b c d)
  "Manage the 4-tile config for the four input tiles.

The loops scan from 0-th row [column] to the second-to-last row [column].  This
is because the 4-tile config is indexed from the top left corner, so this loop
indexing will cover the entire mosaic board.

We do not worry about managing tiles that are not allowed on the edges since
`tiles-and-allowed-edges' will manage that; e.g., if this function places a
variable that encodes T9 (crossing tile) on the edge, then
`tiles-and-allowed-edges' will set that variable to False, and the clause will
be satisifed, and everything will behave normally."
  (loop for i from 0 to (- *m* 2) do
    (loop for j from 0 to (- *n* 2) do
      ;;(format t "~a ~a~%" i j)
      (1-coord-4-clockings i j (list a b c d)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-coords-of-rotational-symmetry (i j)
  "Given a location (i,j), return a list of lists of the neighbor coordinates in
a 2x2 grid if (i,j) is in the upper right, upper left, lower left, and lower
right, respectively.  In other words, build coordinates of rotational symmetry."
  (let ((i- (- i 1))
        (j- (- j 1))
        (i+ (+ i 1))
        (j+ (+ j 1)))

      ;; Start the list
      (list

       ;; Input location at location 1 (upper right)
       (when (and
              (in-bounds i  j-)
              (in-bounds i+ j )
              (in-bounds i+ j-))
         (list
          (list i  j )  ;; input location
          (list i  j-)  ;; directly left
          (list i+ j )  ;; directly below
          (list i+ j-)))  ;; lower left

       ;; Input location at location 2 (upper left)
       (when (and
              (in-bounds i  j+)
              (in-bounds i+ j )
              (in-bounds i+ j+))
         (list
          (list i  j )  ;; input location
          (list i  j+)  ;; directly right
          (list i+ j )  ;; directly below
          (list i+ j+)))  ;; lower right

       ;; Input tile at location 3 (lower left)
       (when (and
              (in-bounds i  j+)
              (in-bounds i- j )
              (in-bounds i- j+))
         (list
          (list i  j )  ;; input location
          (list i  j+)  ;; directly right
          (list i- j )  ;; directly above
          (list i- j+)))  ;; upper right

       ;; Input tile at location 4 (lower right)
       (when (and
              (in-bounds i  j-)
              (in-bounds i- j )
              (in-bounds i- j-))
         (list
          (list i  j )  ;; input location
          (list i  j-)  ;; directly left
          (list i- j )  ;; directly above
          (list i- j-)))  ;; upper left

)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code to produce the entire formula

(defun leading-zero (i)
  "Given an integer i, format it with a leading zero (if needed)."
  (format nil "~2,'0d" i))

(defun timestamp ()
  (multiple-value-bind (sec min hour day month year)
      (get-decoded-time)
    (format nil "~d-~d-~d at ~d:~d:~d"
            (leading-zero year)
            (leading-zero month)
            (leading-zero day)
            (leading-zero hour)
            (leading-zero min)
            (leading-zero sec))))

;;;;;;;;;;;;;;;;

(defun encode-suitably-connected-knot (m-input n-input)
  "Given board dimensions of m x n and assuming t=11 tiles, produce the entire
formula for a suitably connected knot.  To encode additional constraints, write
and use another function."
  ;; Update the m,n values to be the input values
  (defparameter *m* m-input)
  (defparameter *n* n-input)
  (defparameter *t* 11)  ;; exactly 11 tiles for the square, unoriented version

  ;; Print file header info
  (format t "c Board dimensions are ~d x ~d (rows x columns)~%" *m* *n*)
  (format t "c To run: bc_minisat_all_static <input dimacs> <output file>~%")
  (format t "c ~A~%" (timestamp))
  (format t "p cnf ~d ~d~%" (* 11 *m* *n*) 99999999999)  ;; "p cnf <num variables> <num clauses>"

  ;; Encode the 'exactly one tile' constraint for all (i,j) locations.
  (loop for i from 0 to (- *m* 1) do
    (loop for j from 0 to (- *n* 1) do
      (exactly-one-tile i j)))

  ;; Allowed corners
  (tiles-and-allowed-corners)

  ;; Allowed edges
  (tiles-and-allowed-edges "left"   2 3 6)
  (tiles-and-allowed-edges "right"  1 4 6)
  (tiles-and-allowed-edges "top"    1 2 5)
  (tiles-and-allowed-edges "bottom" 3 4 5)

  ;; Allowed neighbors for every variable v, i.e., every (i,j) position with
  ;; every possible k-th tile.  Start at "1" since the functions expect
  ;; variables which are 1-indexed.
  (loop for v from 1 to (* *t* *m* *n*) do
    (print-all-neighbors v))

  ;; Require at least three crossing tiles
  (require-at-least-1-tile-of-this-type  9)
  (require-at-least-1-tile-of-this-type 10)
  (require-at-least-3-crossing-tiles)


  ;;;;;;;;;;;;;;;;
  ;; == "railroad tracks" ==
  ;; i.e., straight lines next to each other
  (all-coords-ban-2x2-configs  5  5  5  5)


  ;;;;;;;;;;;;;;;;
  ;; == "bumps" ==

  (all-coords-ban-2x2-configs  5  2  6  0)  ;; "corner" with blank tile in lower right
  (all-coords-ban-2x2-configs  5  2  6  2)  ;; "corner" with non-connected arc in lower right

  (all-coords-ban-2x2-configs  1  2  6  3)  ;; line (T6) in lower left
  (all-coords-ban-2x2-configs  1  2  4  6)  ;; line (T6) in lower right
  (all-coords-ban-2x2-configs  1  2  6  6)  ;; two lines (T6) in bottom row
  (all-coords-ban-2x2-configs  8  2  3  7)  ;; a "bump" with two single arcs (T2,T3) to the left

  (all-coords-ban-2x2-configs  4  2  3  1)  ;; dump bump

  ;; Caps caused by at least one double arc
  (all-coords-ban-2x2-configs  7  2  6  6)  ;; 1 double arc
  (all-coords-ban-2x2-configs  1  8  6  6)  ;; 1 double arc
  (all-coords-ban-2x2-configs  7  8  6  6)  ;; 2 double arcs


  ;;;;;;;;;;;;;;;;
  ;; == "waists" ==

  (all-coords-ban-2x2-configs  8  2  3  4)
  (all-coords-ban-2x2-configs  8  2  3  8)
  (all-coords-ban-2x2-configs  8  2  7  4)
  (all-coords-ban-2x2-configs  8  2  7  8)
  (all-coords-ban-2x2-configs  8  8  3  4)
  (all-coords-ban-2x2-configs  8  8  3  8)
  (all-coords-ban-2x2-configs  8  8  7  4)
  (all-coords-ban-2x2-configs  8  8  7  8)


  ;;;;;;;;;;;;;;;;
  ;; == Type I crossings ==

  (all-coords-ban-2x2-configs  1  2  3  4)  ;; 2x2 circles

  (all-coords-ban-2x2-configs  7  2  3  4)  ;; 3 single arcs + 1 double arc in upper right
  (all-coords-ban-2x2-configs  7  2  3  8)  ;; 2 single arcs + 2 double arcs
  (all-coords-ban-2x2-configs  7  2  7  4)  ;; 2 single arcs + 2 double arcs on a diagonal
  (all-coords-ban-2x2-configs  1  8  7  8)  ;; 1 single arc  + 3 double arc tiles
  (all-coords-ban-2x2-configs  7  8  7  8)  ;; 0 single arcs + 4 double arc tiles

  (all-coords-ban-2x2-configs  1  2  9  4)  ;; 3 single arcs
  (all-coords-ban-2x2-configs  1  2 10  4)

  ;;;;

  ;; Do the flips with crossing tile T9
  (all-coords-ban-2x2-configs  1  2  3  9)
  (all-coords-ban-2x2-configs  1  2  7  9)
  (all-coords-ban-2x2-configs  1  8  3  9)
  (all-coords-ban-2x2-configs  1  8  7  9)
  (all-coords-ban-2x2-configs  7  2  3  9)
  (all-coords-ban-2x2-configs  7  2  7  9)
  (all-coords-ban-2x2-configs  7  8  3  9)
  (all-coords-ban-2x2-configs  7  8  7  9)

  ;; Do the same operations as above, but replace T9 with T10
  (all-coords-ban-2x2-configs  1  2  3 10)
  (all-coords-ban-2x2-configs  1  2  7 10)
  (all-coords-ban-2x2-configs  1  8  3 10)
  (all-coords-ban-2x2-configs  1  8  7 10)
  (all-coords-ban-2x2-configs  7  2  3 10)
  (all-coords-ban-2x2-configs  7  2  7 10)
  (all-coords-ban-2x2-configs  7  8  3 10)
  (all-coords-ban-2x2-configs  7  8  7 10)


  ;;;;;;;;;;;;;;;;
  ;; == Type II crossings ==

  (all-coords-ban-2x2-configs  1  9  9  4)  ;; 2 single arcs to the right   + 2 crossings (T9) to left
  (all-coords-ban-2x2-configs  7  9  9  4)  ;; 1 double arc on top right    + 2 crossings (T9) to left
  (all-coords-ban-2x2-configs  1  9  9  8)  ;; 1 double arc on bottom right + 2 crossings (T9) to left

  (all-coords-ban-2x2-configs  1 10 10  4)  ;; 2 single arcs to the right   + 2 crossings (T10) to left
  (all-coords-ban-2x2-configs  7 10 10  4)  ;; 1 double arc on top right    + 2 crossings (T10) to left
  (all-coords-ban-2x2-configs  1 10 10  8)  ;; 1 double arc on bottom right + 2 crossings (T10) to left

  (all-coords-ban-2x2-configs  1 10  3  9)  ;; classic Type II

  ;;;;

  ;; Type II crossing with crossing tiles on one diagonal and arc tiles (either
  ;; single or double arcs) on the other diagonal

  (all-coords-ban-2x2-configs  1  9  3  10)
  (all-coords-ban-2x2-configs  1  9  7  10)
  (all-coords-ban-2x2-configs  7  9  3  10)
  (all-coords-ban-2x2-configs  7  9  7  10)


)
