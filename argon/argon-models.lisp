(in-package :sigma&K)

(export '(+gudmundsson-2002+ +HBS+ +phelps+))

(defvar *ar-data-dir*
  (merge-pathnames
   #P"my-software-add-ons/my-lisp/modeling/collision-cross-sections-and-rates/argon/"
   #+WTEHCFMXYP1 #p"/home/977315/"
   #+CYSSHD1 #P"/home/mv/")
  "Path to the Argon data directory")

(defparameter +gudmundsson-2002+ nil
  "Flag for models based on the Gudmundsson 2002 report
RH-21-2002")

(defparameter +HBS+ nil
  "HBS Ar cross-sections 

I think HBS refers to the Berkeley PIC codes")

(defparameter +phelps+ nil
  "Phelps effective Ar cross-sections 

The cross-sections are extracted from the ELECTRONS.TXT file")