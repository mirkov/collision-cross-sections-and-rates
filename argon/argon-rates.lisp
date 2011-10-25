(in-package :sigma&K)




(defmacro defGudmundsson02-rate (name &body body)
  `(defmethod ,name ((model (eql +Gudmundsson-2002+)) Te)
     (assert (and (>= Te 1.0)
		  (<= Te 7.0))
	     (Te) "Te must be in the rage 1 - 7eV" Te)
     (the float
       ,@body)))


(defGudmundsson02-rate K-Ar+e->Ar+e% 
  (* 2.336e-14
       (expt Te 1.609)
       (exp (- (* 0.0618 (^2 (log Te)))
	       (* 0.1171 (^3 (log Te)))))))

(defgudmundsson02-rate K-Ar+e->Ar*+e%
  (K-Arrhenius 2.48e-14 12.78 0.33))

(defGudmundsson02-rate K-Ar+e->Ar/+/+2e% 
  (K-Arrhenius Te 2.34e-14 17.44 0.59))


(defGudmundsson02-rate K-Ar+e->Ar/3P2/+e% 
  (K-Arrhenius Te 5.02e-15 12.64))

(defGudmundsson02-rate K-Ar+e->Ar/3P1/+e% 
  (K-Arrhenius Te 1.91e-15 12.6))

(defGudmundsson02-rate K-Ar+e->Ar/3P0/+e% 
  (K-Arrhenius Te 1.35e15 12.42))

(defGudmundsson02-rate K-Ar+e->Ar/1P1/+e% 
  (K-Arrhenius Te 2.72e-16 12.14))

(defGudmundsson02-rate K-Ar+e->Ar/4p/+e% 
  (K-Arrhenius Te 2.12e-14 13.13))

(defGudmundsson02-rate K-Ar+e->Ar/4s+4s^/+e%
  (K-Arrhenius Te 1.45e-14 12.96))

(defGudmundsson02-rate K-Ar+e->Ar/5s+3d_+5s^+3d^/+e%
  (K-Arrhenius Te 1.22e-14 12.96))

(defGudmundsson02-rate K-Ar+e->Ar/4d+6s+4d_+4d^/+e%
  (K-Arrhenius Te 7.98e-15 19.05))

(defGudmundsson02-rate K-Ar+e->Ar/6s^+5d+72+5d_+higher/+e%
  (K-Arrhenius Te 8.29e-15 18.14))

(defGudmundsson02-rate K-Ar+e->Ar+2e%
  (K-Arrhenius Te 2.34e-14 17.44 0.59))

