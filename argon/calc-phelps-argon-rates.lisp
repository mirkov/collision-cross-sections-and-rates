;; Mirko Vukovic
;; Time-stamp: <2011-11-04 12:23:39EDT calc-phelps-argon-rates.lisp>
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
;; - calculates argon rates by integrating reaction cross-sections
;; - save rates as table
;; - reade tables and set-up interpolation methods




(defun plot-argon+e->momentum ()
  "Plot of argon+e maxwellian electron rates as function of
temperature"
  (let* ((Te (gseq 0.1d0 100d0 101))
	 (K1 (gcmap (calc-rate *phelps-ar+e->qm-ell* :maxwell @!temp) 
		    Te))
	 (K2 (gcmap (calc-rate *phelps-ar+e->qm-tot* :maxwell @!temp) 
		    Te)))
    (set-to ((logscale :xy)
	     (xlabel "Te")
	     (ylabel "m^3/s")
	     (title "Maxwellian electron momentum transfer rate")
	     #|(yrange '(1e-18 1e-12))|#)
      (plot-xys Te `((,K1 :title "Ellastic")
		     (,K2 :title "Total"))))))

(defun plot-argon+e->inellastic ()
  "Plot of argon+e maxwellian electron rates as function of
temperature

This function does not work at the moment because of the grid/gsll
issue"
  (let* ((Te (gseq 0.1d0 100d0 101))
	 (K1 (gcmap (calc-rate *phelps-ar+e->exc* :maxwell @!temp) 
		    Te))
	 (K2 (gcmap (calc-rate *phelps-ar+e->ion* :maxwell @!temp) 
		    Te)))
    (set-to ((logscale :xy)
	     (xlabel "Te")
	     (ylabel "m^3/s")
	     (title "Maxwellian electron inellastic rates")
	     #|(yrange '(1e-18 1e-12))|#)
      (plot-xys Te `((,K1 :title "Excitation")
		     (,K2 :title "Ionization"))))))

(defun write-phelps-argon-rates (sigma-interpolation filename)
  "Calculate a rate table for `sigma-interpolation' and write to
`filename' in the `*Xe-data-dir*'"
  (let* ((table (rate-table sigma-interpolation))
	 (*default-pathname-defaults* *Ar-data-dir*))
    (write-rate-table-to-file table filename)))

#|
 (progn
   (write-phelps-argon-rates *phelps-ar+e->qm-ell* "K-Ar+e->Qm-ell.dat")
   (write-phelps-argon-rates *phelps-ar+e->qm-tot* "K-Ar+e->Qm-tot.dat"))
 (write-phelps-argon-rates *phelps-ar+e->ion* "K-Ar+e->ion.dat")
 (write-phelps-argon-rates *phelps-ar+e->exc* "K-Ar+e->exc.dat")
|#

(defmacro cleanup-Ar-rate-data ()
`(progn
   (unintern '*K-Ar+e->Qm-tot*)
   (unintern '*K-Ar+e->Qm-ell*)
   (unintern '*K-Ar+e->ion*)
   (unintern '*K-Ar+e->exc*)))

#| (cleanup-ar-rate-data) |#
  

(defvar *K-Ar+e->Qm-tot*
  (setup-interpolation
   (merge-pathnames "K-Ar+e->Qm-tot.dat"
		    *Ar-data-dir*))
  "Interpolation data for electron-argon momentum transfer rate for
Maxwellian electrons

The data was obtained by integration of Phelps' recommended 
cross-sections")

(defvar *K-Ar+e->Qm-ell*
  (setup-interpolation
   (merge-pathnames "K-Ar+e->Qm-ell.dat"
		    *Ar-data-dir*))
  "Interpolation data for electron-argon momentum transfer rate for
Maxwellian electrons

The data was obtained by integration of Phelps' recommended 
cross-sections")

(defvar *K-Ar+e->ion*
  (setup-interpolation
   (merge-pathnames "K-Ar+e->ion.dat"
		    *Ar-data-dir*))
  "Interpolation data for argon ionization for Maxwellian electrons

The data was obtained by integration of Phelps' recommended 
cross-sections")

(defvar *K-Ar+e->exc*
  (setup-interpolation
   (merge-pathnames "K-Ar+e->exc.dat"
		    *Ar-data-dir*))
  "Interpolation data for argon excitation rate for Maxwellian
electrons

The data was obtained by integration of Phelps' recommended 
cross-sections")

(defun plot-argon+e-rates ()
  "Plot of argon+e maxwellian electron rates as function of
temperature

It reproduces Fig. 3.16 of L&L"
  (let* ((Te (second *K-Ar+e->ion*))
	 (K-Qm-ell (third *K-Ar+e->qm-ell*))
	 (K-Qm-tot (third *K-Ar+e->qm-tot*))
	 (K-exc (third *K-Ar+e->exc*))
	 (K-ion (third *K-Ar+e->ion*)))
    (set-to ((logscale :xy)
	     (xlabel "Te [Ev]")
	     (ylabel "K [m^3/s]")
	     (title "Electron Argon rates for Maxwellian electron")
	     (xrange '(.1 100))
	     (yrange '(1e-18 1e-12)))
      (plot-xys Te `((,K-Qm-ell :title "Ellastic")
		     (,K-Qm-tot :title "Total")
		     (,K-exc :title "Excitation")
		     (,K-ion :title "Ionization"))))))

(def-Kinterpol-method K-Ar+e->Qm :Phelps-Maxwell *K-Ar+e->Qm-tot*)
(def-Kinterpol-method K-Ar+e->Qm-ell :Phelps-Maxwell *K-Ar+e->Qm-ell*)
(def-Kinterpol-method K-Ar+e->exc :Phelps-Maxwell *K-Ar+e->exc*)
(def-Kinterpol-method K-Ar+e->ion :Phelps-Maxwell *K-Ar+e->ion*)
