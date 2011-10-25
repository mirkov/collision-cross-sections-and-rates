(in-package :sigma&K)

;;;; Argon cross-sections from xpdp documentation

(defmacro cx-1-val-call (&rest args)
  (let ((E_0 (second args)))
    `(values (cx-1 ,(first args) ,E_0 ,@(subseq args 2)) ,E_0)))

    
(defmethod sigma-Ar+e->Ar+2e% ((model (eql +HBS+)) eV)
  "Ar Ionization cross section and threshold"
  (cx-1-val-call eV 15.76d0 6.5d-17 0.5d0 0.87d0 1.4d0 ))


(defmethod sigma-Ar+e->Ar_r+e% ((model (eql +HBS+)) eV)
  "Ar Excitation to lumped radiative levels cross-section and threshold"
  (cx-1-val-call eV 11.62d0 1.1d-17 0.18d0 0.85d0))

(defmethod sigma-Ar+e->Ar*+e% ((model (eql +HBS+)) eV)
  "Ar Excitation to lumped metastable levels and threshold"
  (cx-1-val-call eV 11.55d0 3.5d-18 8d0 2d0))


(defmethod sigma-Ar+e->Ar+e% ((model (eql +HBS+)) eV)
  "Ar-el ellastic cross-section and energies of Ramsauer minimum and
maximum cx"
  (let ((E_0 0.345d0)
	(E_1 12d0))
    (values
     (cond
       ((< eV 0.345d0) (* 8.3d-22 (expt eV -1.25d0)))
       ((< eV 12d0) (* 1.1d-20  (expt eV 1.26d0)))
       (t (* 1.2d-18 (expt eV -0.67d0))))
     E_0 E_1)))

