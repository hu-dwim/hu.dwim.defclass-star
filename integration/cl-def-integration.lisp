;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :defclass-star)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :cl-def :defclass-star))

;; TODO this is too much like build-defclass-like-expansion, factor out
(def (definer :available-flags "eas") class* (name supers slots &rest options)
  (unless (eq (symbol-package name) *package*)
    (style-warn "def class* for ~A while its home package is not *package* (~A)"
                (let ((*package* (find-package "KEYWORD")))
                  (format nil "~S" name)) *package*))
  (let ((*accessor-names* nil)
        (*slot-names* nil))
    (multiple-value-bind (binding-names binding-values clean-options)
        (extract-options-into-bindings options)
      (progv binding-names (mapcar #'eval binding-values)
        (let* ((*export-class-name-p* (getf -options- :export
                                            *export-class-name-p*))
               (*export-accessor-names-p* (getf -options- :export-accessor-names
                                                *export-accessor-names-p*))
               (*export-slot-names-p* (getf -options- :export-slot-names
                                            *export-slot-names-p*))
               (result `(defclass ,name ,supers
                          ,(mapcar 'process-slot-definition slots)
                          ,@clean-options)))
          (if (or *export-class-name-p*
                  *export-accessor-names-p*
                  *export-slot-names-p*)
              `(progn
                 ,result
                 (eval-when (:compile-toplevel :load-toplevel :execute)
                   (export (list ,@(mapcar (lambda (el)
                                             (list 'quote el))
                                           (append (when *export-class-name-p*
                                                     (list name))
                                                   (when *export-accessor-names-p*
                                                     (nreverse *accessor-names*))
                                                   (when *export-slot-names-p*
                                                     (nreverse *slot-names*)))))
                           ,*package*))
                 (find-class ',name nil))
              result))))))

(integrated-export 'class* :cl-def)

#|

(def (class* eas) foo (super)
  ((slot1 42)))

|#
