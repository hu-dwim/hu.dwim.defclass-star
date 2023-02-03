;;;; SPDX-FileCopyrightText: hu.dwim & Atlas Engineer LLC
;;;; SPDX-License-Identifier: Public Domain

(in-package :common-lisp-user)

(defpackage :nclass/test
  (:use :cl :nclass :lisp-unit2))

(defpackage :nclass/test.dummy)

(defpackage :nclass/test.pkg1
  (:use :nclass))

(defpackage :nclass/test.pkg2
  (:use :nclass))
