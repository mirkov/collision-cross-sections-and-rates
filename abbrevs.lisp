(in-package :sigma&K)

(define-symbol-macro +eps0+ physics-constants:+free-space-permittivity-SP+)
(define-symbol-macro +a0+ physics-constants:+bohr-radius-SP+)
(define-symbol-macro +mp+ physics-constants:+proton-mass-SP+)
(define-symbol-macro +e+ physics-constants:+elementary-charge-SP+)
(define-symbol-macro +kb+ physics-constants:+boltzmann-constant-sp+)

(defconstant +pi+ (coerce pi 'double-float)
  "constant pi as double float")