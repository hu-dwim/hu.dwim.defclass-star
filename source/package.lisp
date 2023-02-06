;;;; SPDX-FileCopyrightText: hu.dwim & Atlas Engineer LLC
;;;; SPDX-License-Identifier: Public Domain

(in-package :common-lisp-user)

(defpackage :nclass
  (:use :common-lisp)
  (:export #:define-class
           #:define-condition*
           ;; transformers
           #:default-accessor-name-transformer
           #:dwim-accessor-name-transformer
           #:default-initarg-name-transformer
           #:default-slot-definition-transformer
           #:default-predicate-name-transformer
           #:always-dashed-predicate-name-transformer
           #:question-mark-predicate-name-transformer
           #:make-name-transformer
           #:*allowed-slot-definition-properties*))
