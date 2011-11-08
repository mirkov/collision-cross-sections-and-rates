;; Mirko Vukovic
;; Time-stamp: <2011-11-04 12:29:14EDT xenon-generic.lisp>
;; 
;; Copyright 2011 Mirko Vukovic
;; Distributed under the terms of the GNU General Public License
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :sigma&k)

(export '(K-Xe+e->Qm K-Xe+e->exc K-Xe+e->ion))

(defgeneric K-Xe+e->Qm (model Te)
  (:documentation "Electron - Xenon total momentum transfer
collision rate"))

(defgeneric K-Xe+e->exc (model Te)
  (:documentation "Electron - Xenon effective excitation rate"))

(defgeneric K-Xe+e->ion (model Te)
  (:documentation "Electron - Xenon ionization rate"))


