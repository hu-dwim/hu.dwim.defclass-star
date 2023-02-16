;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: Public Domain

(defsystem "nclasses"
  :version "0.2.1"
  :description "Simplify class like definitions with define-class and friends."
  :author "dwim.hu & Atlas Engineer LLC"
  :maintainer "Atlas Engineer LLC"
  :homepage "https://github.com/atlas-engineer/nclasses"
  :license "Public Domain"
  :depends-on (moptilities)
  :in-order-to ((test-op (test-op "nclasses/tests")
                         (test-op "nclasses/tests/compilation")))
  :serial t
  :pathname "source/"
  :components ((:file "package")
               (:file "duplicates")
               (:file "defclass-star")))

(defsystem "nclasses/submodules"
  :defsystem-depends-on ("nasdf")
  :class :nasdf-submodule-system)

(defsystem "nclasses/tests"
  :defsystem-depends-on ("nasdf")
  :class :nasdf-test-system
  :depends-on (nclasses moptilities)
  :targets (:package :nclasses/test)
  :pathname "test/"
  :components ((:file "package")
               (:file "test" :depends-on ("package"))))

(defsystem "nclasses/tests/compilation"
  :defsystem-depends-on ("nasdf")
  :class :nasdf-compilation-test-system
  :depends-on (nclasses)
  :packages (:nclasses))
