(defpackage :atomic-processes-unit-tests
  (:use :common-lisp :lisp-unit :my-lisp-unit
	:atomic-processes :my-utils :my-nlisp-utils))

(in-package :atomic-processes-unit-tests)

;; I pick the intervals and test values of F and D so that I can use
;; the numeric values in *pwr-law-vals* and *exp-law-vals* in my
;; verification routines.

;; creates grid points at 0, 

;; helper functions for mode picking
(defvar *mode* 'plot
  "Complex comp mode.  Can be print, plot, write benchmark data to file, or check with benchmark data from file")

(defmacro mode-equal (test)
  `(eq *mode* ,test))
(defun print-mode-p ()
  (mode-equal 'print))
(defun plot-mode-p ()
  (mode-equal 'plot))
(defun write-mode-p ()
  (mode-equal 'write))
(defun check-mode-p ()
  (mode-equal 'check))

(define-test ar-drift-velocity
  (assert-equal (/ 4 (expt (1+ (expt 0.007 1.5)) 0.33))
		(ar-drift-velocity 1)))

