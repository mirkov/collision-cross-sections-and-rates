(defpackage :collision-cross-sections-and-rates
  (:nicknames :sigma&K)
  (:use :cl :my-utils :physics-constants :lisp-unit
	:mv-grid
	:mv-gnuplot)
  (:import-from :alexandria
		:with-input-from-file)
  (:export
   :el+ar-ellastic
   :el+ar->2el+ar^+
   :sigma-l
   :K-l
   :rel-polarizability
   :polarizability
   :sigma-cx-res
   :ar-drift-velocity :ar-ion-neutral-coll-freq
   :e+Ar-ellastic-rate
   :Td))


(defpackage :sigma&K-user
  (:use :cl :sigma&K :mv-gnuplot))
#|

(defpackage :abbrevs.atomic-processes
  (:use :cl :physics-constants)
  (:export :+eps0+ :+a0+ :+mp+ :+e+))

(defpackage :utilities.atomic-processes
  (:use :cl :physics-constants)
  (:export :m* :Td :cx-1))

(defpackage :hbs-Ar.atomic-processes
  (:nicknames :hbs-Ar)
  (:use :cl :my-utils
	:abbrevs.atomic-processes
	:utilities.atomic-processes)
  (:export :Ar+el->Ar+2el :Ar+el->Ar_r+el :Ar+el->Ar*+el :Ar+el->Ar+el))

(defpackage :hbs-Ar-unit-tests.atomic-processes
  (:use :cl :lisp-unit :mv-gnuplot :hbs-Ar.atomic-processes))

|#