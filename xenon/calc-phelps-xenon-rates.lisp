;; Mirko Vukovic
;; Time-stamp: <2011-11-04 16:34:36EDT calc-phelps-xenon-rates.lisp>
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
;; Functionality:
;; - calculates Xenon rates by integrating reaction cross-sections
;; - save rates as table
;; - reade tables and set-up interpolation methods

(export '(energy-loss/Xe+))

(defun write-phelps-xenon-rates (sigma-interpolation filename)
  "Calculate a rate table for `sigma-interpolation' and write to
`filename' in the `*Xe-data-dir*'"
  (let* ((table (rate-table sigma-interpolation))
	 (*default-pathname-defaults* *Xe-data-dir*))
    (write-rate-table-to-file table filename)))


#|
(write-phelps-xenon-rates *phelps-xe+e->qm* "K-Xe+e->Qm.dat")
(write-phelps-xenon-rates *phelps-xe+e->ion* "K-Xe+e->ion.dat")
(write-phelps-xenon-rates *phelps-xe+e->exc* "K-Xe+e->exc.dat")
|#

(defmacro cleanup-xe-rate-data ()
`(progn
   (unintern '*K-Xe+e->Qm*)
   (unintern '*K-Xe+e->ion*)
   (unintern '*K-Xe+e->exc*)))

(defvar *K-Xe+e->Qm*
  (setup-interpolation
   (merge-pathnames "K-Xe+e->Qm.dat"
		    *Xe-data-dir*))
  "Interpolation data for electron-xenon momentum transfer rate for
Maxwellian electrons

The data was obtained by integration of Phelps' recommended 
cross-sections")

(defvar *K-Xe+e->exc*
  (setup-interpolation
   (merge-pathnames "K-Xe+e->exc.dat"
		    *Xe-data-dir*))
  "Interpolation data for xenon excitation rate for Maxwellian
electrons

The data was obtained by integration of Phelps' recommended 
cross-sections")

(defvar *K-Xe+e->ion*
  (setup-interpolation
   (merge-pathnames "K-Xe+e->ion.dat"
		    *Xe-data-dir*))
  "Interpolation data for xenon ionization for Maxwellian electrons

The data was obtained by integration of Phelps' recommended 
cross-sections")



(defun plot-xenon+e-rates ()
  "Plot of xenon+e maxwellian electron rates as function of
temperature

It reproduces Fig. 3.16 of L&L"
  (let* ((Te (second *K-Xe+e->ion*))
	 (K-Qm (third *K-Xe+e->qm*))
	 (K-exc (third *K-Xe+e->exc*))
	 (K-ion (third *K-Xe+e->ion*)))
    (set-to ((logscale :xy)
	     (xlabel "Te")
	     (ylabel "m^3/s")
	     (title "Electron-Xenon rates for Maxwellian electrons")
	     (xrange '(.1 100))
	     (yrange '(1e-18 1e-12)))
      (plot-xys Te `((,K-Qm :title "Ellastic")
		     (,K-exc :title "Excitation")
		     (,K-ion :title "Ionization"))))))

(def-Kinterpol-method K-Xe+e->Qm :Phelps-Maxwell *K-Xe+e->Qm*)
(def-Kinterpol-method K-Xe+e->exc :Phelps-Maxwell *K-Xe+e->exc*)
(def-Kinterpol-method K-Xe+e->ion :Phelps-Maxwell *K-Xe+e->ion*)

(defun energy-loss/Xe+ (Te)
  (let ((Kion (K-Xe+e->ion :phelps-maxwell Te))
	(Kexc (K-Xe+e->exc :phelps-maxwell Te))
	(Km (K-Xe+e->Qm :phelps-maxwell Te)))
    (energy-loss/ion Te (Eiz :Xe) Kion (Eexc :Xe) Kexc Km 131.29)))