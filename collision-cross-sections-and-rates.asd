(asdf:defsystem collision-cross-sections-and-rates
    :name "collision-cross-sections-and-rates"
    :author "Mirko Vukovic <mirko.vukovic@gmail.com>"
    :version "0.1"
    :description "Mirko's atomic processes library"
    :serial t
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
	      ((:file "atomic-processes")))
     (:module "argon"
	      :depends-on ("base")
	      :serial t
	      :components ((:file "argon-models")
			   (:file "argon-tresholds")
			   (:file "argon-generic")
			   (:file "argon-rates")
			   (:file "hbs-Ar-cross-sections")
			   (:file "hbs-Ar-cross-sections-unit-tests")
			   (:file "argon-ion-neutral"))))
    :depends-on (:alexandria
		 :gsll
		 :lisp-unit
		 :physics-constants
		 :mv-grid-utils
		 :mv-gnuplot
		 :my-utils))


(asdf:defsystem collision-cross-sections-and-rates-user
    :name "collision-cross-sections-and-rates-user"
    :author "Mirko Vukovic <mirko.vukovic@gmail.com>"
    :version "0.1"
    :description "Mirko's atomic processes library"
    :serial t
    :components
    ((:module "base"
	      :pathname #p"./"
	      :components ((:file "atomic-processes-package-def")))
     (:module "argon"
	      :components ((:file "hbs-Ar-corss-sections-unit-tests"))))
    :depends-on (:collision-cross-sections-and-rates
		 :mv-gnuplot))
