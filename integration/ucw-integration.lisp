;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :defclass-star)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :cl-def))

(defmacro defcomponent* (name supers slots &rest options)
  (build-defclass-like-expansion
   name supers slots options
   (lambda (processed-slots clean-options)
     `(ucw:defcomponent ,name ,supers
        ,processed-slots
        ,@clean-options))))

(def (definer :available-flags "eas") component* (name supers slots &rest class-options)
  (build-defclass-like-cl-def-expansion
   name supers slots class-options -options-
   (lambda (processed-slots clean-options)
     `(ucw:defcomponent ,name ,supers
        ,processed-slots
        ,@clean-options))))

(integrated-export 'defcomponent* :ucw)
(integrated-export 'component* :ucw)

(pushnew :component *allowed-slot-definition-properties*)
(pushnew :backtrack *allowed-slot-definition-properties*)
