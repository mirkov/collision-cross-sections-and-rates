;; Mirko Vukovic
;; Time-stamp: <2011-11-03 15:46:37EDT edfs.lisp>
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

(defgeneric electron-edf (type energy temperature &rest rest)
  (:documentation "Electron energy distribution function

The distribution type is specified by a keyword such as :maxwell
or :dryvestein or others

function of `energy' (eV), `temperature' (eV) and other
parameters"))

(defmethod electron-edf ((type (eql :maxwell)) ev temp &rest rest)
  "Maxwellian electron energy distribution function

Based on L&L 18.1.2 converted to eV instead of velocity"
  (declare (ignore rest))
  (* (expt (/ +electron-mass-sp+
	      (* 2 pi +elementary-charge-sp+ temp))
	   1.5)
     (exp (- (/ eV  temp)))))
