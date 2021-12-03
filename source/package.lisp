;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.defclass-star
  (:nicknames :class*)
  (:use :common-lisp)
  (:export #:defclass*
           #:defcondition*
           ;; transformers
           #:default-accessor-name-transformer
           #:dwim-accessor-name-transformer
           #:default-initarg-name-transformer
           #:default-slot-definition-transformer
           #:default-predicate-name-transformer
           #:make-name-transformer
           #:*allowed-slot-definition-properties*))
