;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.defclass-star/test
  (:use :hu.dwim.common
        :hu.dwim.defclass-star
        :hu.dwim.stefil))

(defpackage :hu.dwim.defclass-star/test.dummy)

(defpackage :hu.dwim.defclass-star/test.pkg1
  (:use :hu.dwim.common
        :hu.dwim.defclass-star
        :hu.dwim.stefil))

(defpackage :hu.dwim.defclass-star/test.pkg2
  (:use :hu.dwim.common
        :hu.dwim.defclass-star
        :hu.dwim.stefil))
