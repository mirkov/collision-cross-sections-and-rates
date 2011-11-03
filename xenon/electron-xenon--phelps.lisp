;; Mirko Vukovic
;; Time-stamp: <2011-11-03 17:34:41EDT electron-xenon--phelps.lisp>
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
;; Facilities:
;; - reading of Phelps' Xenon cross-sections
;; - Setup of cross-section interpolation
;; - Plotting of Xe cross-sections

(defvar *phelps-Xe+e->Qm*
  (setup-interpolation
   (merge-pathnames "Xe+e--Qm.dat"
		    *Xe-data-dir*)))

(defvar *phelps-Xe+e->ion*
  (setup-interpolation
   (merge-pathnames "Xe+e--ion.dat"
		    *Xe-data-dir*)))

(defvar *phelps-Xe+e->exc*
  (setup-interpolation
   (merge-pathnames "Xe+e--exc.dat"
		    *Xe-data-dir*)))



(def-sigmainterpol-method sigma-Xe+e->Qm% *phelps-Xe+e->Qm* )
(def-sigmainterpol-method sigma-Xe+e->ion% *phelps-Xe+e->ion*)
(def-sigmainterpol-method sigma-Xe+e->exc% *phelps-Xe+e->exc*)




(def-ratecalc-method K-Xe+e->Qm% *phelps-Xe+e->qm* )
(def-ratecalc-method K-Xe+e->exc% *phelps-Xe+e->exc* )
(def-ratecalc-method K-Xe+e->ion% *phelps-Xe+e->ion* )

(defun plot-phelps-xenon+e-sigma ()
  "Plot of xenon+e cross-section as function of energy

It reproduces Fig. 3.16 of L&L"
  (let* ((E0 (second *Phelps-Xe+e->qm*))
	 (Qm (third *Phelps-Xe+e->qm*))
	 (E2 (second *Phelps-Xe+e->exc*))
	 (exc (third *Phelps-Xe+e->exc*))
	 (E3 (second *Phelps-Xe+e->ion*))
	 (ion (third *Phelps-Xe+e->ion*)))
    (set-to ((logscale :xy)
	     (xlabel "eV")
	     (ylabel "m^2")
	     (title "electron-Xenon cross-sections")
	     #|(xrange '(.1 100))
	     (yrange '(1e-21 nil))|#)
      (plot-xyps  `((,E0 ,Qm :title "Ellastic")
		     (,E2 ,exc :title "Excitation")
		     (,E3 ,ion :title "Ionization"))))))