(in-package :sigma&K)

(export '(sigma-Ar+e->Ar+e% sigma-Ar+e->Ar_r+e% sigma-Ar+e->Ar*+e%
	  sigma-Ar+e->Ar/+/+2e% K-Ar+e->Ar+e% K-Ar+e->Ar*+e%
	  K-Ar+e->Ar/+/+2e% K-e+ar->2e+ar^+% K-Ar+e->Ar/3P2/+e%
	  K-Ar+e->Ar/3P1/+e% K-Ar+e->Ar/3P0/+e% K-Ar+e->Ar/1P1/+e%
	  K-Ar+e->Ar/4p/+e% K-Ar+e->Ar/4s+4s^/+e%
	  K-Ar+e->Ar/5s+3d_+5s^+3d^/+e% K-Ar+e->Ar/4d+6s+4d_+4d^/+e%
	  K-Ar+e->Ar/6s^+5d+72+5d_+higher/+e% K-Ar+e->Ar+2e%))

(defgeneric sigma-Ar+e->Ar+e% (model eV)
  (:documentation "Ar-el ellastic cross-section"))

(defgeneric sigma-Ar+e->Ar_r+e% (model eV)
  (:documentation "Ar Excitation to lumped radiative levels cross-section"))

(defgeneric sigma-Ar+e->Ar*+e% (model eV)
  (:documentation "Ar Excitation to lumped metastable levels"))

(defgeneric sigma-Ar+e->Ar/+/+2e% (model eV)
  (:documentation "Ar Ionization cross section"))


(defgeneric K-Ar+e->Ar+e% (model Te)
  (:documentation "Electron - Argon ellastic collision rate"))

(defgeneric K-Ar+e->Ar*+e% (model Te)
  (:documentation "Electron - Argon metastable collision rate"))

(defgeneric K-Ar+e->Ar/+/+e% (model Te)
  (:documentation "Electron-Argon ionization rate"))
(defgeneric K-Ar+e->Ar/3P2/+e% (model Te))
(defgeneric K-Ar+e->Ar/3P1/+e% (model Te))
(defgeneric K-Ar+e->Ar/3P0/+e% (model Te))
(defgeneric K-Ar+e->Ar/1P1/+e% (model Te))
(defgeneric K-Ar+e->Ar/4p/+e% (model Te))
(defgeneric K-Ar+e->Ar/4s+4s^/+e% (model Te))
(defgeneric K-Ar+e->Ar/5s+3d_+5s^+3d^/+e% (model Te))
(defgeneric K-Ar+e->Ar/4d+6s+4d_+4d^/+e% (model Te))
(defgeneric K-Ar+e->Ar/6s^+5d+72+5d_+higher/+e% (model Te))
(defgeneric K-Ar+e->Ar+2e% (model Te))

