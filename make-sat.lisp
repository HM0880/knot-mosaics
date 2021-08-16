;;;; Call the general SAT encoding function (in "main.lisp").  Also can encode
;;;; additional constraints by running commands after the general function.
;;;; To run:
;;;;   sbcl --script make-sat.lisp > OUTFILE
;;;; 2021-04-05

(load "main.lisp")  ;; custom function for SAT encoding (lives in this directory)

;; Define the board size here since "main.lisp" has its own values for
;; debugging
(defparameter *m* 10)  ;; rows
(defparameter *n* 10)  ;; columns

;; Call the function to make the SAT formula
(encode-suitably-connected-knot *m* *n*)


;;;;;;;;;;;;;;;;
;; Additional constraints go here

;; Encode the only possible layout for a space-efficient 5-mosaic per
;; "Tile Number and Space-Efficient Knot Mosaics (2018).pdf" (p. 13).

;; Row 0
(format t "~d 0~%" (tup2var 0 0 0))
(format t "~d 0~%" (tup2var 0 1 2))
(format t "~d 0~%" (tup2var 0 2 1))
(format t "~d 0~%" (tup2var 0 3 0))
(format t "~d 0~%" (tup2var 0 4 0))

;; Row 1
(format t "~d 0~%" (tup2var 1 0 2))
;; skip Column 1
;; skip Column 2
(format t "~d 0~%" (tup2var 1 3 1))
(format t "~d 0~%" (tup2var 1 4 0))

;; Row 2
(format t "~d 0~%" (tup2var 2 0 3))
;; skip Column 1
;; skip Column 2
;; skip Column 3
(format t "~d 0~%" (tup2var 2 4 1))

;; Row 3
(format t "~d 0~%" (tup2var 3 0 0))
(format t "~d 0~%" (tup2var 3 1 3))
;; skip Column 2
;; skip Column 3
(format t "~d 0~%" (tup2var 3 4 4))

;; Row 4
(format t "~d 0~%" (tup2var 4 0 0))
(format t "~d 0~%" (tup2var 4 1 0))
(format t "~d 0~%" (tup2var 4 2 3))
(format t "~d 0~%" (tup2var 4 3 4))
(format t "~d 0~%" (tup2var 4 4 0))
