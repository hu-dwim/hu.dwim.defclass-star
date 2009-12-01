;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.defclass-star+hu.dwim.computed-class
  :class hu.dwim.system
  :depends-on (:hu.dwim.computed-class
               :hu.dwim.defclass-star)
  :components ((:module "integration"
                :components ((:file "computed-class")))))
