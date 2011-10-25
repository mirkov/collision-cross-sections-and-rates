(in-package :sigma&K-user)


(define-plot ar-cx-comp-plot
  (set-to ((logscale :xy)
	   (yrange '(1e-21 1e-18))
	   (xlabel "Energy [eV]")
	   (ylabel "[m^2]")
	   (title "Electron-Ar cross-sections -- HBS curve fits"))
    (plot-funs `((,#'sigma-ar+e->ar/+/+2e% :title "Ionization")
		 (,#'sigma-ar+e->ar+e% :title "Elastic")
		 (,#'sigma-ar+e->ar_r+e% :title "Radiative")
		 (,#'sigma-ar+e->ar*+e% :title "Metastable"))
	       0.1d0 3000 101 t)))
