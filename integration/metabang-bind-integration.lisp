;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :defclass-star)

(defmacro define-dynamic-context* (name direct-slots &rest args
                                        &key (defclass-macro-name 'defclass*)
                                        &allow-other-keys)
  (remf-keywords args :defclass-macro-name)
  `(metabang.bind:define-dynamic-context ,name ,direct-slots
    :defclass-macro-name ,defclass-macro-name
    ,@args))

(integrated-export 'define-dynamic-context* :metabang-bind)

