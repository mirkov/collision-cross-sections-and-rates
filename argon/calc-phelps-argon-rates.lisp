;; Mirko Vukovic
;; Time-stamp: <2011-11-03 16:59:33EDT calc-argon-rates.lisp>
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

(defun write-rates1 (filename Te K)
  (alexandria:with-output-to-file
      (stream (merge-pathnames filename
			       *ar-data-dir*)
	      :if-exists :supersede)
    (let ((dim-Te (grid:dim0 Te))
	  (dim-K (grid:dim0 K)))
      (assert (= dim-Te dim-K)
	      () "Vectors must be of same length")
      (prin1 dim-Te stream)
      (dotimes (i dim-Te)
	(print i stream)
	(prin1 (grid:gref Te i) stream)
	(write-char #\space stream)
	(prin1 (grid:gref K i) stream)))))


(defun write-rates (cross-section-table filename)
  (let* ((Te (gseq 0.1d0 100d0 101))
	 (K (gcmap (calc-rate cross-section-table :maxwell @!temp)
		   Te))
	 (K1 (gmap (lambda (K)
		     (* 1e20 K))
		   K)))
    (write-rates1 filename Te K1)))

#|
(progn
  (write-rates *phelps-ar+e->qm-ell* "K-Ar+e->Qm-ell.dat")
  (write-rates *phelps-ar+e->qm-tot* "K-Ar+e->Qm-tot.dat"))
(write-rates *phelps-ar+e->ion* "K-Ar+e->ion.dat")
(write-rates *phelps-ar+e->exc* "K-Ar+e->exc.dat")
|#

  
  

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
	     (xlabel "Te")
	     (ylabel "m^3/s")
	     (title "Maxwellian electron momentum transfer rate")
	     (xrange '(.1 100))
	     (yrange '(1e-18 1e-12)))
      (plot-xys Te `((,K-Qm-ell :title "Ellastic")
		     (,K-Qm-tot :title "Total")
		     (,K-exc :title "Excitation")
		     (,K-ion :title "Ionization"))))))