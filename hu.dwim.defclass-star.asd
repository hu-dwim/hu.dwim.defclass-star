;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.defclass-star
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "Simplify class like definitions with defclass* and friends."
  :version (:read-file-form "version.sexp")
  :components ((:module "source"
                :components ((:file "package")
                             (:file "duplicates" :depends-on ("package"))
                             (:file "defclass-star" :depends-on ("duplicates"))))))

(defsystem :hu.dwim.defclass-star/test
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :version (:read-file-form "version.sexp")
  :depends-on (:hu.dwim.common
               :hu.dwim.defclass-star
               :hu.dwim.stefil+hu.dwim.def+swank)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "test" :depends-on ("package"))))))

(defsystem :hu.dwim.defclass-star/documentation
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.documentation-system"
  :version (:read-file-form "version.sexp")
  :depends-on (:hu.dwim.defclass-star.test
               :hu.dwim.presentation)
  :components ((:module "documentation"
                :components ((:file "defclass-star" :depends-on ("package"))
                             (:file "package")))))
