;; Mirko Vukovic
;; Time-stamp: <2011-11-04 16:40:56EDT example-plots.lisp>
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

(in-package :sigma&k-user)

(defun plot-xenon+e-rates ()
  "Plot of xenon+e maxwellian electron rates as function of
temperature

It reproduces Fig. 3.16 of L&L"
    (set-to ((logscale :xy)
	     (xlabel "Te")
	     (ylabel "m^3/s")
	     (title "Electron-Xenon rates for Maxwellian electrons")
	     (xrange '(.1 100))
	     (yrange '(1e-18 1e-12)))
      (plot-funs  (list (list #'(lambda (Te)
				  (K-Xe+e->Qm :phelps-maxwell Te))
			      :title "Ellastic")
			(list #'(lambda (Te)
				  (K-Xe+e->exc :phelps-maxwell Te))
			      :title "Excitation")
			(list #'(lambda (Te)
				  (K-Xe+e->ion :phelps-maxwell Te))
			      :title "Ionization"))
		    0.1001d0 99.99d0 120 t)))

(defun plot-argon+e-rates ()
  "Plot of argon+e maxwellian electron rates as function of
temperature

It reproduces Fig. 3.16 of L&L"
    (set-to ((logscale :xy)
	     (xlabel "Te")
	     (ylabel "m^3/s")
	     (title "Electron-Argon rates for Maxwellian electrons")
	     (xrange '(.1 100))
	     (yrange '(1e-18 1e-12)))
      (plot-funs  (list (list #'(lambda (Te)
				  (K-Ar+e->Qm :phelps-maxwell Te))
			      :title "Momentum")
			(list #'(lambda (Te)
				  (K-Ar+e->Qm-ell :phelps-maxwell Te))
			      :title "Momentum-ellastic")
			(list #'(lambda (Te)
				  (K-Ar+e->exc :phelps-maxwell Te))
			      :title "Excitation")
			(list #'(lambda (Te)
				  (K-Ar+e->ion :phelps-maxwell Te))
			      :title "Ionization"))
		    0.1001d0 99.99d0 120 t)))
(defun plot-energy-loss-curves ()
  "Generate plot of energy loss/ion created for Ar & Xe

This replicates Figure 3.17, but for O for which I currently don't
have implemented the rates"
  (set-to ((logscale :xy)
	   (xlabel "Te")
	   (ylabel "eV")
	   (title "Energy loss per ion created"))
    (plot-funs `((energy-loss/ar+ :title "Ar")
		 (energy-loss/Xe+ :title "Xe"))
	       1d0 99.99d0 101 t)))