;;;; SPDX-FileCopyrightText: hu.dwim & Atlas Engineer LLC
;;;; SPDX-License-Identifier: Public Domain

(in-package :common-lisp-user)

(defpackage :nclasses
  (:use :common-lisp)
  (:export #:define-class
           #:define-condition*
           #:define-generic
           ;; transformers
           #:default-accessor-name-transformer
           #:dwim-accessor-name-transformer
           #:question-mark-accessor-name-transformer
           #:default-initarg-name-transformer
           #:default-slot-definition-transformer
           #:default-predicate-name-transformer
           #:always-dashed-predicate-name-transformer
           #:question-mark-predicate-name-transformer
           #:default-type-inference
           #:make-name-transformer
           #:*allowed-slot-definition-properties*)
  (:documentation "This library offers two helper macros:
- `nclasses:define-class'
- `nclasses:define-condition*'.

Compared to the standard macros, they accept extra options and slot definition
is smarter.

Example:

(define-class foo ()
  ((slot1 :initarg nil)
   (slot2 \"hello!\")
   (unexported-slot :export nil))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer #'nclasses:default-accessor-name-transformer))

In the above, all slot accessors are automatically defined using
`nclasses:default-accessor-name-transformer'.  They are also exported together with the
class name.
The initarg default to the keyword version of the slot symbol, unless it's
explicitly set to NIL.

Notice that the second value of the slot definition, if not an option, is then
the initform.

See `nclasses:define-class' for more details."))
