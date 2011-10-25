(in-package :sigma&K)

(defun m* (m1 m2)
  (/ (* m1 m2)
     (+ m1 m2)))

(defun Td (E n &optional (SI nil))
  "Conversion from E (V/cm) and n (cm^-3) to Townsends.
If parameter SI is set to t, E and n are in SI units"
  (* (/ E n)
     (if SI 1d21 1d17)))

(defun cx-1 (E E_0 sigma_0 &optional (a 1d0) (b 0d0) (c 1d0))
  "Generic formula for reactions with treshold `E_0' as function of
normalized energy `x': E/E_0.  XPDP documentation,
p. 21"
  (if (< E E_0) 0d0
      (let ((x (/ E E_0)))
	(* (/ sigma_0
	      (* (expt E_0 2)
		 (expt x b)))
	   (expt (/ (- (* a x) a)
		    (+ (* a x) 1d0))
		 c)))))

(defun K-arrhenius (Te C1 C2 &optional (C3 0))
  "Evaluate
C1 * Te^C3 (exp (- Te / C2 ) )"
  (* C1 (expt Te C3) (exp (- (/ C2 Te)))))