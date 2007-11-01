;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :defclass-star)

(defmacro defcomponent* (name supers slots &rest options)
  (build-defclass-like-expansion
   name supers slots options
   (lambda (processed-slots clean-options)
     `(ucw:defcomponent ,name ,supers
        ,processed-slots
        ,@clean-options))))

(integrated-export 'defcomponent* :ucw)

(pushnew :component *allowed-slot-definition-properties*)
(pushnew :backtrack *allowed-slot-definition-properties*)
