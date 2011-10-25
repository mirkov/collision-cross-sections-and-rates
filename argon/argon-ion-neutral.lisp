(in-package :sigma&K)

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




