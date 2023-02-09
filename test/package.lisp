;;;; SPDX-FileCopyrightText: hu.dwim & Atlas Engineer LLC
;;;; SPDX-License-Identifier: Public Domain

(in-package :common-lisp-user)

(defpackage :nclasses/test
  (:use :cl :nclasses :lisp-unit2))

(defpackage :nclasses/test.dummy)

(defpackage :nclasses/test.pkg1
  (:use :nclasses))

(defpackage :nclasses/test.pkg2
  (:use :nclasses))
