;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.defclass-star+hu.dwim.def+contextl
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :depends-on (:hu.dwim.defclass-star+contextl
               :hu.dwim.defclass-star+hu.dwim.def)
  :components ((:module "integration"
                :components ((:file "def+contextl")))))
