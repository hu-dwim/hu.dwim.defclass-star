;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :defclass-star)

(defmacro deflayer* (name supers slots &rest options)
  (build-defclass-like-expansion
   name supers slots options
   (lambda (processed-slots clean-options)
     `(contextl:deflayer ,name ,supers
        ,processed-slots
        ,@clean-options))))

(integrated-export 'deflayer* :contextl)

(pushnew :special *allowed-slot-definition-properties*)
