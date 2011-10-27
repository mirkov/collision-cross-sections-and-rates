(in-package :sigma&k)

(defparameter *xe-data*
  (merge-pathnames
   #P"my-software-add-ons/my-lisp/modeling/collision-cross-sections-and-rates/xenon/"
   #+WTEHCFMXYP1 #p"/home/977315/"
   #+CYSSHD1 #P"/home/mv/")
  "Path to the Xenon data directory")

(defun read-electron-data (stream rows)
  (read-grid `(,rows 3) stream t :eof-error-p t))

(defun read-Qm-data ()
  (alexandria:with-input-from-file
    (stream
     (merge-pathnames "e-xe--Qm.dat"
		      *xe-data*))
  (read-electron-data stream 66)))

(defun read-ionization-data ()
  (alexandria:with-input-from-file
    (stream
     (merge-pathnames "e-xe--ionization.dat"
		      *xe-data*))
    (read-line stream)
    (read-electron-data stream 27)))

(defun read-single-level-excitation-data ()
  (alexandria:with-input-from-file
    (stream
     (merge-pathnames "e-xe--single-level-excitation.dat"
		      *xe-data*))
    (read-line stream)
    (read-electron-data stream 27)))

(defun calc-rate (data f temp)
  (let* ((e (grid:column data 1))
	 (sigma (grid:column data 2))
	 (f-val (gmap #'(lambda (e)
			  (funcall f e temp))
		      e))
	 (integrand (grid:map-n-grids :sources
				      `((,e nil)
					(,sigma nil)
					(,f-val nil))
				      :combination-function
				      (lambda (e sigma f)
					(* e sigma f)))))
    (let ((interp (gsll:make-interpolation gsll:+linear-interpolation+
					   e integrand)
	    #|(gsll:make-spline gsll:+cubic-spline-interpolation+
					   e integrand)|#)
	  (e-min (grid:gref e 0))
	  (e-max (grid:gref e (1- (grid:dim0 e)))))
      #|(gsll:evaluate interp (* 0.5d0 (+ e-min e-max))
		     :xa e :ya integrand)|#
      (gsll:evaluate-integral interp e-min e-max
			      :xa e :ya integrand))))

(defun maxwell-edf (e temp)
  (exp (- (/ e temp))))

(let ((data (read-single-level-excitation-data))
      (temp 1d0))
  (calc-rate data #'maxwell-edf temp))