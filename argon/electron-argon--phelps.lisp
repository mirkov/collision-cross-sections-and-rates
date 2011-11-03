;; Mirko Vukovic
;; Time-stamp: <2011-11-03 17:24:41EDT electron-argon--phelps.lisp>
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

(defvar *phelps-Ar+e->Qm-tot*
  (setup-interpolation
   (merge-pathnames "Ar+e--Qm-tot.dat"
		    *Ar-data-dir*)))

(defvar *phelps-Ar+e->Qm-ell*
  (setup-interpolation
   (merge-pathnames "Ar+e--Qm-ell.dat"
		    *Ar-data-dir*)))

(defvar *phelps-Ar+e->ion*
  (setup-interpolation
   (merge-pathnames "Ar+e--ion.dat"
		    *Ar-data-dir*)))

(defvar *phelps-Ar+e->exc*
  (setup-interpolation
   (merge-pathnames "Ar+e--exc.dat"
		    *Ar-data-dir*)))



(def-sigmainterpol-method sigma-Ar+e->Qm-ell% *phelps-Ar+e->Qm-ell*)
(def-sigmainterpol-method sigma-Ar+e->Qm-tot% *phelps-Ar+e->Qm-tot*)
(def-sigmainterpol-method sigma-Ar+e->ion% *phelps-Ar+e->ion*)
(def-sigmainterpol-method sigma-Ar+e->exc% *phelps-Ar+e->exc*)

#|
(let ((data (read-single-level-excitation-data))
      (temp 1d0))
  (calc-rate data #'maxwell-edf temp))|#





(def-ratecalc-method K-Ar+e->Qm-ell% *phelps-ar+e->qm-ell* )

(defun plot-phelps-argon+e-sigma ()
  "Plot of argon+e cross-section as function of energy

It reproduces Fig. 3.16 of L&L"
  (let* ((E0 (second *Phelps-Ar+e->qm-ell*))
	 (Qm-ell (third *Phelps-Ar+e->qm-ell*))
	 (E1 (second *Phelps-Ar+e->qm-tot*))
	 (Qm-tot (third *Phelps-Ar+e->qm-tot*))
	 (E2 (second *Phelps-Ar+e->exc*))
	 (exc (third *Phelps-Ar+e->exc*))
	 (E3 (second *Phelps-Ar+e->ion*))
	 (ion (third *Phelps-Ar+e->ion*)))
    (set-to ((logscale :xy)
	     (xlabel "eV")
	     (ylabel "m^2")
	     (title "electron-Argon cross-sections")
	     #|(xrange '(.1 100))|#
	     (yrange '(1e-22 nil)))
      (plot-xyps  `((,E0 ,Qm-ell :title "Ellastic")
		     (,E1 ,Qm-tot :title "Total")
		     (,E2 ,exc :title "Excitation")
		     (,E3 ,ion :title "Ionization"))))))