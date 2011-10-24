(in-package :atomic-processes)



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
	   

;; Polarizability values (L&L Table 3.2 quoting Smirnov, 1981) in
;; units of 1/bohr-radius^.
;; 
;; Usage example (cdr (assoc 'Ar *alpha-r*))
(defparameter *alpha-r*
  (list 
   (cons :H 4.5)
   (cons :C 12.)
   (cons :N 7.5)
   (cons :O 5.4)
   (cons :Ar 11.08)
   (cons :C-Cl4 69.)
   (cons :C-F4 19.)
   (cons :C-O 13.2)
   (cons :C-O2 17.5)
   (cons :Cl2 31.)
   (cons :H2-O 9.8)
   (cons :N-H3 14.8)
   (cons :O2 10.6)
   (cons :S-F6 30.))
  "Relative polarizabilities")

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
  

(defun ar-drift-velocity (Td)
  "Argon drift velocity fit by Phelps and Petrovic as function of E/n in Townsends
http://jilawww.colorado.edu/~avp/collision_data/ionneutral/iontrans.txt"
  (/ (* 4d0 Td)
     (expt (+ 1d0
	      (expt (* 0.007d0 Td) 1.5d0))
	   0.33d0)))

(defun ar-ion-neutral-coll-freq (Td n)
  "Argon ion-neutral collision frequency Derived from the
experimentally obtained ar-drift velocity.  See notes in
ar-drift-velocity."
  (* (/ +e+ (* 40 +mp+))
     2.5d-22 n (expt (1+ (expt (* 7d-3 Td)
			1.5d0))
		     0.33d0)))



(defun e+Ar-ellastic-rate (Te)
  "Electron ion ellastic collision rate.
LL2 -- Table 3.3.
Original data: Gudmundsson 2002.  See References in LL2"
  (assert (and (> Te 1.0)
	       (< Te 7.0))
	  (Te) "Te must be in the rage 1 - 7eV" Te)
  (the float
    (* 2.336e-14
       (expt Te 1.609)
       (exp (- (* 0.0618 (^2 (log Te)))
	       (* 0.1171 (^3 (log Te))))))))


(defun el+ar->2el+ar^+ (Te)
  "Electron-Argon ionization rate
LL2 -- Table 3.3.
Original data: Gudmundsson 2002.  See References in LL2"
  (assert (or (> Te 1.0)
	    (< Te 7.0))
	  (Te) "Te must be in the rage 1 - 7eV" Te)
  (* 2.34e-14 (expt Te 0.59) (exp (0- (/ 17.44 Te)))))


