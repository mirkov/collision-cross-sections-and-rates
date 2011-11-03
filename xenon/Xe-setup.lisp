;; Mirko Vukovic
;; Time-stamp: <2011-11-03 17:21:15EDT Xe-setup.lisp>
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

(defvar *xe-data-dir*
  (merge-pathnames
   #P"my-software-add-ons/my-lisp/modeling/collision-cross-sections-and-rates/xenon/"
   #+WTEHCFMXYP1 #p"/home/977315/"
   #+CYSSHD1 #P"/home/mv/")
  "Path to the Xenon data directory")