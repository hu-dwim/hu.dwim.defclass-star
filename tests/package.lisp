;;;; SPDX-FileCopyrightText: hu.dwim & Atlas Engineer LLC
;;;; SPDX-License-Identifier: Public Domain

(in-package :cl-user)

(defpackage :nclasses/tests
  (:use :cl :nclasses :lisp-unit2))

(defpackage :nclasses/test.dummy)

(defpackage :nclasses/test.pkg1
  (:use :nclasses))

(defpackage :nclasses/test.pkg2
  (:use :nclasses))
