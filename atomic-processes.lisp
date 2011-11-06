(in-package :sigma&K)



;;; Ellectron collision rates
;;; Rate expressions from Lieberman & Lichtenberg, table 3.3



;;; ellastic charged-particle - netural scattering

;; L&L 2nd ed. (3.3.13)
(defun sigma-l (alpha-p q mr vr)
  "Langevin cross-section
alpha-p - polarizability [m^-3]
q - charge [C]
mr - reduced mass [kg]
vr - relative velocity [m/s]"
     (/ (K-l alpha-p q mr) vr))

;; L&L 2nd ed. (3.3.15)
(defun K-l (alpha-p q mr)
  "Langevin collision rate
alpha-p - polarizability [m^-3]
q - charge [C]
mr - reduced mass [kg]
Lieberman & Lichtenberg 2nd ed, (eq 3.3.15)"
  (sqrt (/ (* pi alpha-p (^2 q))
	   (* +eps0+ mr))))


(define-test alpha-r
  (let ((lisp-unit:*epsilon* 1e-2))
    ;; compare value quoted in Lieberman with one calculated from CRC
    ;; HCP, 79th Ed, p. 10-162
    (assert-number-equal 11.06
			 (alpha-r 1.6411e-30))))

(defun alpha-r (alpha)
  "Calculate relative polarizability from absolute polarizability
`alpha'

`alpha' is in units m^-3"
  (/ alpha (expt +a0+ 3)))


	   

;; Polarizability values (L&L Table 3.2 quoting Smirnov, 1981) in
;; units of 1/bohr-radius^ and from other sources
;; 
;; Usage example (assoc 'Ar *alpha-r*)
(defparameter *alpha-r*
  `((:H . 4.5)
    (:C . 12.)
    (:N . 7.5)
    (:O . 5.4)
    (:Ar . 11.08)
    (:Xe . ,(alpha-r 4.044e-30))
    (:C-Cl4 . 69.)
    (:C-F4 . 19.)
    (:C-O . 13.2)
    (:C-O2 . 17.5)
    (:Cl2 . 31.)
    (:H2-O . 9.8)
    (:N-H3 . 14.8)
    (:O2 . 10.6)
    (:S-F6 . 30.))
  "Relative polarizabilities obtained from L&L, Table 3.2 and CRC Handbook of Chemistry and Physics, 79th Edition, p. 10-162")

(defun rel-polarizability (id)
  "relative molecule polarizability.  Usage: (rel-polarizability :ar)"
  (or (cdr (assoc id
		  *alpha-r*))
      (error "~a not found" id)))

(defun polarizability (id)
  "Molecule polarizability.  Usage: (polarizability :ar)"
  (* (rel-polarizability id)
     (^3 +a0+)))

(defparameter *sigma-cx-res*
  (list 
   (cons :H '(6.2 5.0))
   (cons :C '(6.2 5.0))  ; same as H
   (cons :N '(4.9 2.8))
   (cons :O '(5.2 4.3))
   (cons :Ar '(5.5 4.5)))
  "Resonant charge exchange cross section at 0.1 and 10 eV in 10^-15 cm^2
Smirnov, Appendix 9")

(defun sigma-cx-res (id &optional 10eV-p)
  (* (funcall (if 10eV-p #'caddr
		  #'cadr)
	      (or (assoc id
			 *sigma-cx-res*)
		  (error "~a not found" id)))
     1e-19))
  

(defparameter *Eion*
  `((:H . 13.598)
    (:C . 11.260)
    (:N . 14.534)
    (:O . 13.618)
    (:Ar . 15.760)
    (:Xe . 12.130))
  "Ionization potentials of atoms (Smirnov, Appendix 10 table)")

(defun Eion (id)
  "Ionization  potential  Usage: (Eion :ar)"
  (or (cdr (assoc id
		  *Eion*))
      (error "~a not found" id)))


(defparameter *Eexc*
  `((:Ar . 11.5)
    (:Xe . 8.32))
  "Ionization treshold of atoms (Smirnov, Appendix 10 table)")

(defun Eexc (id)
  "Excitation treshold  Usage: (Eexc :ar)"
  (or (cdr (assoc id
		  *Eexc*))
      (error "~a not found" id)))