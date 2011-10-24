(asdf:defsystem collision-cross-sections-and-rates
    :name "collision-cross-sections-and-rates"
    :author "Mirko Vukovic <mirko.vukovic@gmail.com>"
    :version "0.1"
    :description "Mirko's atomic processes library"
    :components
    ((:module "common"
	      :pathname #p"./"
	      :components
	      ((:file "atomic-processes-package-def")
	       (:file "abbrevs"
		      :depends-on ("atomic-processes-package-def"))
	       (:file "utilities"
		      :depends-on ("atomic-processes-package-def"
				   "abbrevs"))))
     (:module "base"
	      :pathname #p"./"
	      :depends-on ("common")
	      :components
	      ((:file "atomic-processes")
	       (:file "hbs-Ar-cross-sections")
	       (:file "hbs-Ar-cross-sections-unit-tests"
		      :depends-on ("hbs-Ar-cross-sections")))))
    :depends-on (:my-utils
		 :physics-constants
		 :lisp-unit
		 :mv-gnuplot))
