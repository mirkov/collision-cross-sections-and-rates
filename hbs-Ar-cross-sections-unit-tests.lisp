(in-package :hbs-Ar-unit-tests.atomic-processes)


(define-plot ar-cx-comp-plot
  (set-to ((logscale :xy)
	   (yrange '(1e-21 1e-18))
	   (xlabel "Energy [eV]")
	   (ylabel "[m^2]")
	   (title "Electron-Ar cross-sections -- HBS curve fits"))
    (plot-funs `((,#'hbs-ar.atomic-processes:ar+el->ar+2el :title "Ionization")
		 (,#'hbs-ar.atomic-processes:ar+el->ar+el :title "Elastic")
		 (,#'hbs-ar.atomic-processes:ar+el->ar_r+el :title "Radiative")
		 (,#'hbs-ar.atomic-processes:ar+el->ar*+el :title "Metastable"))
	       0.1d0 3000 101 t)))
