;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: Public Domain

(defsystem "nclasses"
  :version "0.6.0"
  :description "Simplify class like definitions with define-class and friends."
  :author "dwim.hu & Atlas Engineer LLC"
  :maintainer "Atlas Engineer LLC"
  :homepage "https://github.com/atlas-engineer/nclasses"
  :bug-tracker "https://github.com/atlas-engineer/nclasses/issues"
  :source-control (:git "https://github.com/atlas-engineer/nclasses.git")
  :license "Public Domain"
  :depends-on ("moptilities")
  :serial t
  :pathname "source/"
  :components ((:file "package")
               (:file "duplicates")
               (:file "defclass-star"))
  :in-order-to ((test-op (test-op "nclasses/tests"))))

(defsystem "nclasses/tests"
  :depends-on ("nclasses" "lisp-unit2")
  :serial t
  :pathname "tests/"
  :components ((:file "package")
               (:file "tests"))
  :perform (test-op (op c)
                    (eval-input
                     "(lisp-unit2:run-tests
                       :package :nclasses/tests
                       :run-contexts #'lisp-unit2:with-summary-context)")))
