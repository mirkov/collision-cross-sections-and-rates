;; Mirko Vukovic
;; Time-stamp: <2011-11-03 17:24:12EDT phelps-data.lisp>
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
;; utilities for reading, interpolating and processing
;; Art Phelps' data

(defparameter *sigma-interpolation-method*
  gsll:+linear-interpolation+
  "Default intepolation method")


(defun read-phelps-electron-data (file)
  (alexandria:with-input-from-file
      (stream file)
    (let ((rows (read stream)))
  (read-grid `(,rows 3) stream t :eof-error-p t))))

(defun setup-interpolation1 (e sigma e-min e-max)
  "Setup an interpolation method between e & sigma.  e-min and e-max
are the domain of e"
  (list (gsll:make-interpolation *sigma-interpolation-method*
				 e sigma)
	e sigma e-min e-max))

(defun setup-interpolation (file)
  "Setup interpolation variables for `file'

`lines' is the number of file records"
  (let* ((table (read-phelps-electron-data file))
	 (e (grid:column table 1))
	 (sigma (gmap #'(lambda (sigma)
			  (* 1e-20 sigma))
		      (grid:column table 2)))
	 (e-min (grid:gref e 0))
	 (e-max (grid:gref e (- (grid:dim0 e) 1))))
    (setup-interpolation1 e sigma e-min e-max)))

(defmacro def-sigmainterpol-method (name interpolation-data)
  "Define a method `name' for interpolating `interpolation-data'

`interpolation-data' is a list created by `setup-interpolation'.  It
consists of
- a GSLL interpolation object
- energy scale (in eV)
- cross-section (m2)
- minimum energy
- maximum energy"
  `(defmethod ,name ((model (eql :phelps)) coll-e)
     (destructuring-bind (method e sigma e-min e-max) ,interpolation-data
       (assert (<= coll-e e-max)
	       ()
	       "Collision energy ~a is above the tabulated range ~a"
	       coll-e e-max)
       (if (<= coll-e e-min) 0d0
	   (gsll:evaluate method coll-e :xa e :ya sigma)))))

(defmacro def-ratecalc-method (name interpolation-data)
  "Define a method `name' for interpolating `interpolation-data'

`interpolation-data' is a list created by `setup-interpolation'.  It
consists of
- a GSLL interpolation object
- energy scale (in eV)
- cross-section (m2)
- minimum energy
- maximum energy"
  `(defmethod ,name ((model (eql :phelps)) edf temp)
       (calc-rate ,interpolation-data edf temp)))