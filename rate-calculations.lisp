;; Mirko Vukovic
;; Time-stamp: <2011-11-08 11:56:03 rate-calculations.lisp>
;; 
;; Copyright 2011 Mirko Vukovic
;; Distributed under the terms of the GNU General Public License
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :sigma&k)

(defparameter *rate-interpolation-method*
  gsll:+linear-interpolation+
  "Default interpolation method for calculating rates via
integration")

(defun calc-rate (interpolation-data edf temp)
  "Using the tabulated electron reaction cross-section data in
`interpolation-data' calculate rates for the energy distribution
function `edf' for given temperature `temp'

Energy units are in electron volts

The normalization factor is derived in generic-works."
  (print 0)
  (destructuring-bind (method e sigma e-min e-max)
      interpolation-data
    (declare (ignore method))
    (let* ((f-val (gmap #'(lambda (e)
			    (funcall #'electron-edf edf e temp))
			e))
	   (integrand (grid:map-n-grids :sources
					(list (list e nil)
					      (list sigma nil)
					      (list f-val nil))
					:combination-function
					(lambda (e sigma f)
					  (* e sigma f)))))
      (let ((interp (gsll:make-interpolation
		     gsll:+linear-interpolation+
		     e integrand)))
	(* 8d0 +pi+ (expt (/ +elementary-charge-sp+
			   +electron-mass-sp+)
			2d0)
	   (gsll:evaluate-integral interp e-min e-max
				   :xa e :ya integrand))))))

#|
(let ((data (read-single-level-excitation-data))
      (temp 1d0))
  (calc-rate data #'maxwell-edf temp))|#

(defun K-sigma-constant (sigma Te &optional (e-min 0d0))
  "Maxwellian reaction rate for a constant `sigma' at temperature `Te'
sigma is zero below e-min

Derived in generic-works"
  (let ((c 6.6924e5))
    (* c sigma (sqrt Te) (exp (- (/ e-min Te))) (+ 1 (/ e-min Te)))))

(define-test uniform-sigma
  (let ((e-min 0d0)
	(e-max 100d0)
	(count 1001)
	(lisp-unit:*epsilon* 1e-2)
	(ref 6.6924e-15))
    (let ((e (lseq e-min e-max count))
	  (sigma (useq 1d-20 count)))
      (let ((sigma-data (setup-interpolation1 e sigma e-min e-max)))
	(assert-number-equal ref
			     (calc-rate sigma-data :maxwell 1d0))
	(assert-number-equal (* ref 2)
			     (calc-rate sigma-data :maxwell 4d0))
	(assert-number-equal (* ref 3)
			     (calc-rate sigma-data :maxwell 9d0))))))


(define-test heaviside-sigma
  (let ((e-min 10d0)
	(e-max 100d0)
	(count 1001)
	(lisp-unit:*epsilon* 1e-2))
    (let ((e (lseq e-min e-max count))
	  (sigma (useq 1d-20 count)))
      (let ((sigma-data (setup-interpolation1 e sigma e-min e-max)))
	
	(assert-number-equal (K-sigma-constant 1e-20 1d0 10d0)
			     (calc-rate sigma-data :maxwell 1d0))
	(assert-number-equal (K-sigma-constant 1e-20 4d0 10d0)
			     (calc-rate sigma-data :maxwell 4d0))
	(assert-number-equal (K-sigma-constant 1e-20 9d0 10d0)
			     (calc-rate sigma-data :maxwell 9d0))))))



(defun foo (x y)
  "Demonstrates the problem of map-n-grids and
gsll:make-interpolation"
  (let ((z (grid:map-n-grids  :combination-function (lambda (y)
						      y)
			      :sources `((,y nil)))))
    (gsll:make-interpolation gsll:+linear-interpolation+
			     x z)))

#|
;; code that demonstrates failure
(let ((x #m(1d0 2d0 3d0))
      (y #m(1d0 1d0 1d0))
      (u #m(1d0 2d0 3d0 4d0))
      (v #m(1d0 1d0 1d0 1d0)))
  (foo x y)
  (foo u v))
|#

(defun make-rate-table (sigma-interpolation &key (edf :maxwell)
		   (T-min 0.1d0) (T-max 100d0) (count 101))
  "Create a rate interpolation table for cross-sections
defined by the `sigma-interpolation' list.

The table is returned as a 2D grid with Te in the first column, and K
in the second.

The electron distribution is specified by the keyword `edf' and
defaults to :maxwell.  In principle, it can accept keywords such
as :dryvestein (although that edf is not implemented yet)

The tabulation is done on a log-sequence of `count' points between
`T-min' and `T-max'"
  (let* ((Tes (gseq T-min T-max count))
	 (table (grid:make-grid `((grid:foreign-array ,count 2) double-float))))
    (dotimes (i count)
      (let ((Te (gref Tes i)))
	(setf (gref table i 0) Te
	      (gref table i 1)
	      (calc-rate sigma-interpolation edf Te))))
    table))

(defun print-rate-table (Te/K-table &optional (stream t))
  "Print the rate table (a 2D grid) in a three column format where
each row contains the row index, Te value and K value

The first emitted line is the number of rows."
  (let ((count (dim0 Te/K-table)))
    (prin1 count stream)
    (dotimes (i count)
      (print i stream)
      (prin1 (grid:gref Te/K-table i 0) stream)
      (write-char #\space stream)
      (prin1 (* 1e20 (grid:gref Te/K-table i 1)) stream))))

(defun write-rate-table-to-file (table file)
  (with-output-to-file (stream file :if-exists :supersede)
    (print-rate-table table stream)))


(defun energy-loss/ion (Te Eiz Kiz Eex Kex Kel M)
  "Equation for energy loss for each ion created"
  (/ (+ (* Eiz Kiz)
	(* Eex Kex)
	(* (/ (* 3 +electron-mass-sp+)
	      (* M +proton-mass-sp+))
	   Te Kel))
     Kiz))