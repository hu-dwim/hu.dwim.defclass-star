;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :defclass-star)

(when (member metabang.bind:*defclass-macro-name-for-dynamic-context*
              `(common-lisp:defclass ,(ignore-errors (read-from-string "metatilities:defclass*"))))
  (setf metabang.bind:*defclass-macro-name-for-dynamic-context* 'defclass-star:defclass*)
  (warn "metabang.bind:*defclass-macro-name-for-dynamic-context* has been set to 'defclass-star:defclass*"))

