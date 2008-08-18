;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :defclass-star)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :cl-def :defclass-star))

(def (definer :available-flags "eas") layer* (name supers slots &rest class-options)
  (build-defclass-like-cl-def-expansion
   name supers slots class-options -options-
   (lambda (processed-slots clean-options)
     `(deflayer ,name ,supers
        ,processed-slots
        ,@clean-options))))

(integrated-export 'layer* :cl-def)

#|

(def (layer* eas) foo (super)
  ((slot1 42 :export :slot)))

|#
