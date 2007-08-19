;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :defclass-star)

(defmacro defcclass* (name superclasses slots &rest options)
  `(defclass-star:defclass* ,name ,superclasses
    ,slots
    ,@(append (unless (find :metaclass options :key 'first)
                '((:metaclass computed-class:computed-class)))
              options)))

(integrated-export 'defcclass* :computed-class)

(mapc (lambda (option)
        (pushnew option *allowed-slot-definition-properties*))
      '(:computed-in :compute-as :slot-value-function :setf-slot-value-function))
