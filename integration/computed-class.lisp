;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.defclass-star)

(defmacro defcclass* (name superclasses slots &rest options)
  `(hu.dwim.defclass-star:defclass* ,name ,superclasses
    ,slots
    ,@(append (unless (find :metaclass options :key 'first)
                '((:metaclass hu.dwim.computed-class:computed-class)))
              options)))

(integrated-export 'defcclass* :hu.dwim.computed-class)

(mapc (lambda (option)
        (pushnew option *allowed-slot-definition-properties*))
      '(:computed-in :compute-as :slot-value-function :setf-slot-value-function))
