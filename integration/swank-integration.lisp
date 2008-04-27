;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :defclass-star)

(unless (assoc "DEFCLASS-STAR" swank:*readtable-alist* :test #'string=)
  (let ((*readtable* (copy-readtable)))
    (enable-sharp-boolean-syntax)
    (push (cons "DEFCLASS-STAR" *readtable*) swank:*readtable-alist*)
    (push (cons "DEFCLASS-STAR.TEST" *readtable*) swank:*readtable-alist*)))
