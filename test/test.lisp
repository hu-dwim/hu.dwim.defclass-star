;;;; SPDX-FileCopyrightText: hu.dwim & Atlas Engineer LLC
;;;; SPDX-License-Identifier: Public Domain

(in-package :nclasses/test)

(defun with-test-class-options (thunk)
  "A context sets the class options to specific defaults."
  (let ((*package* #.*package*)
        (nclasses::*automatic-accessors-p* t)
        (nclasses::*accessor-name-transformer* 'dwim-accessor-name-transformer)
        (nclasses::*automatic-initargs-p* t)
        (nclasses::*initarg-name-transformer* 'default-initarg-name-transformer)
        (nclasses::*export-class-name-p* nil)
        (nclasses::*export-accessor-names-p* nil)
        (nclasses::*export-slot-names-p* nil)
        (nclasses::*automatic-predicates-p* nil)
        (nclasses::*automatic-types-p* nil))
    (funcall thunk)))

(defun with-type-inheritence-class-options (thunk)
  "A context sets the class options to specific defaults."
  (let ((*package* #.*package*)
        (nclasses::*automatic-accessors-p* t)
        (nclasses::*accessor-name-transformer* 'dwim-accessor-name-transformer)
        (nclasses::*automatic-initargs-p* t)
        (nclasses::*initarg-name-transformer* 'default-initarg-name-transformer)
        (nclasses::*export-class-name-p* nil)
        (nclasses::*export-accessor-names-p* nil)
        (nclasses::*export-slot-names-p* nil)
        (nclasses::*automatic-predicates-p* nil)
        (nclasses::*automatic-types-p* t))
    (funcall thunk)))

(defmacro assert-slot= (expansion form &rest extras)
  `(let ((nclasses::*accessor-names* nil)
         (nclasses::*slot-names* nil))
     (assert-equal
      ',expansion
      (nclasses::process-slot-definition ',form)
      ,@extras)))

(defmacro assert-slot-warns (form &rest extras)
  `(let ((nclasses::*accessor-names* nil)
         (nclasses::*slot-names* nil))
     (assert-warning 'warning
                     (nclasses::process-slot-definition ',form) ,@extras)))

(defmacro assert-slot-errors (form &rest extras)
  `(let ((nclasses::*accessor-names* nil)
         (nclasses::*slot-names* nil))
     (assert-error 'error
                   (nclasses::process-slot-definition ',form) ,@extras)))

(define-class foo ()
  ((name "fooname")))

(define-test simple-class ()
  (assert-string= "fooname"
                  (let ((foo (make-instance 'foo)))
                    (name foo))))

(define-class bar ()
  ((name "fooname")
   (age :accessor this-age)
   (address :accessor nil))
  (:accessor-name-transformer (lambda (name def) (declare (ignore def)) name)))

(define-test simple-class-with-custom-accessors ()
  (make-instance 'bar)
  (assert-true (fboundp 'name))
  (assert-true (fboundp 'this-age))
  (assert-false (fboundp 'address)))

(define-class foo-no-accessors ()
  ((name-no-acc :type string))
  (:automatic-accessors-p nil))

(define-test no-accessor ()
  (assert-false (progn
                  (make-instance 'foo-no-accessors)
                  (fboundp 'name-no-acc))))

(define-test nop (:contexts '(with-test-class-options))
  (assert-expands
   (defclass some-class (some super classes)
     ()
     (1 2)
     (3 4))
   (define-class some-class (some super classes)
     ()
     (1 2)
     (3 4))))

(define-test accessors (:contexts '(with-test-class-options))
  (assert-slot= (slot1 :accessor slot1-custom :initarg slot1-custom)
                (slot1 :unbound :accessor slot1-custom :initarg slot1-custom))
  (assert-slot= (slot1 :accessor slot1-of :initarg :slot1)
                (slot1))
  (assert-slot= (slot1 :accessor slot1-p :initarg :slot1 :type boolean)
                (slot1 :unbound :type boolean))
  (assert-slot= (slotp :accessor slotp :initarg :slotp :type boolean)
                (slotp :unbound :type boolean))
  (assert-slot= (slot-name :accessor slot-name-p :initarg :slot-name :type boolean)
                (slot-name :unbound :type boolean))
  (let ((nclasses::*automatic-accessors-p* nil)
        (nclasses::*automatic-initargs-p* nil))
    (assert-slot= (slot1)
                  (slot1)))
  (let ((nclasses::*accessor-name-transformer* (make-name-transformer "FOO-" name "-BAR"))
        (nclasses::*initarg-name-transformer* (make-name-transformer "BAZ-" name)))
    (assert-slot= (slot1 :accessor foo-slot1-bar :initarg baz-slot1)
                  (slot1))))

(define-test reconfiguration (:contexts '(with-test-class-options))
  (assert-expands (defclass some-class (some super classes)
                    ((slot1 :accessor slot1-zork :initarg :slot1))
                    (1 2)
                    (3 4))
                  (define-class some-class (some super classes)
                      ((slot1))
                      (1 2)
                      (:accessor-name-transformer (make-name-transformer name "-ZORK"))
                      (3 4)))
  (assert-expands (defclass some-class (some super classes)
                    ((slot1 :accessor nclasses/test.dummy::slot1-of :initarg :slot1)))
                  (define-class some-class (some super classes)
                      ((slot1))
                      (:accessor-name-package (find-package :nclasses/test.dummy)))))

(define-test full (:contexts '(with-test-class-options))
  (assert-expands (defclass some-class (some super classes)
                    ((slot1 :accessor slot1-of :initarg :slot1 :documentation "zork"))
                    (1 2)
                    (3 4))
                  (define-class some-class (some super classes)
                      ((slot1 :unbound :documentation "zork"))
                      (1 2)
                      (3 4)))
  (assert-expands (defclass some-class (some super classes)
                    ((slot1 :initform 42 :documentation "zork"))
                    (1 2)
                    (3 4))
                  (define-class some-class (some super classes)
                      ((slot1 42 :documentation "zork"))
                      (1 2)
                      (:automatic-accessors-p nil)
                      (:automatic-initargs-p nil)
                      (3 4)))
  (assert-equal `(progn
                  (defclass some-class (some super classes)
                    ((slot1 :initform 42 :accessor slot1-of :initarg :slot1 :documentation "zork")
                     (slot2 :accessor slot2-custom :initarg :slot2)
                     (slot3 :initarg :slot3)))
                  (eval-when (:compile-toplevel :load-toplevel :execute)
                    (export '(some-class slot2 slot2-custom slot1 slot1-of) ,(package-name *package*)))
                  (find-class 'some-class nil))
                (macroexpand-1 '(define-class some-class (some super classes)
                                    ((slot1 42 :documentation "zork")
                                     (slot2 :unbound :accessor slot2-custom)
                                     (slot3 :unbound :accessor nil :export :accessor))
                                    (:export-accessor-names-p t)
                                    (:export-class-name-p t)
                                    (:export-slot-names-p t))))
  (assert-equal `(progn
                   (defclass some-class (some super classes)
                     ((slot1 :initform 42 :accessor slot1-of :initarg :slot1 :documentation "zork")
                      (slot2 :accessor slot2-custom :initarg :slot2)
                      (slot3 :initform 42 :accessor slot3-of :initarg :slot3)
                      (slot4 :initform 42 :accessor slot4-of :initarg :slot4)))
                   (eval-when (:compile-toplevel :load-toplevel :execute)
                     (export '(some-class slot3-of slot2 slot2-custom slot1) ,(package-name *package*)))
                   (find-class 'some-class nil))
                (macroexpand-1 '(define-class some-class (some super classes)
                                 ((slot1 42 :documentation "zork" :export :slot)
                                  (slot2 :unbound :accessor slot2-custom)
                                  (slot3 42 :export :accessor)
                                  (slot4 42 :export nil))
                                 (:export-accessor-names-p t)
                                 (:export-class-name-p t)
                                 (:export-slot-names-p t)))))

(define-test warnings-and-errors (:contexts '(with-test-class-options))
  (let ((*allowed-slot-definition-properties* *allowed-slot-definition-properties*))
    (push :asdf *allowed-slot-definition-properties*)
    (push :unspecified *allowed-slot-definition-properties*)
    (assert-slot= (slot1 :accessor slot1-of :initarg :slot1 :asdf 42 :unspecified 43)
                  (slot1 :asdf 42 :unspecified 43)))
  (assert-slot-warns (slot1 :unbound :asdf slot1-custom))
  (assert-slot-warns (slot1 :asdf slot1-custom))
  (assert-slot-errors (slot1 :unbound foo bar))
  (assert-slot-errors (slot1 foo bar))

  (assert-error 'error
                (macroexpand-1 '(define-class some-class ()
                                 ((slot1))
                                 (:accessor-name-transformer 'default-accessor-name-transformer many)))))

(define-test skip-export-slot-name-from-foreign-package (:contexts '(with-test-class-options))
  (let* ((pkg1 (find-package :nclasses/test.pkg1))
         (pkg2 (find-package :nclasses/test.pkg2))
         (exp1 (let ((*package* pkg1))
                 (macroexpand-1
                  '(define-class nclasses/test.pkg1::foo ()
                    ((nclasses/test.pkg1::foo-name))
                    (:export-slot-names-p nil)))))
         (exp2 (let ((*package* pkg2))
                 (macroexpand-1
                  '(define-class nclasses/test.pkg2::bar
                    (nclasses/test.pkg1::foo)
                    ;; override the slot in the superclass
                    ((nclasses/test.pkg1::foo-name :initform ""))
                    (:export-slot-names-p t))))))
    (let ((*package* pkg1))
      (eval exp1))
    (let ((*package* pkg2))
      (eval exp2))
    (assert-eq :internal
               (nth-value 1 (find-symbol "FOO-NAME" :nclasses/test.pkg1)))
    ;; REVIEW: Is the following the right way to re-initialize the packages?
    (delete-package pkg1)
    (delete-package pkg2)
    (defpackage :nclasses/test.pkg1
      (:use :nclasses))
    (defpackage :nclasses/test.pkg2
      (:use :nclasses))))

(defvar street-name "bar")
(define-test type-inference ()
  (define-class foo-type-infer ()
    ((name "foo")
     (nickname street-name)
     (age 1)
     (height 2.0)
     (width 2 :type number)
     (lisper t)
     (nil-is-not-bool nil)
     (empty-list '())
     (nonempty-list '(1 2 3))
     (mark :foo)
     (sym 'sims)
     (fun #'list)
     (composite (error "Should not eval, type should not be inferred")))
    (:automatic-types-p t))
  (assert-eq 'string
             (getf (mopu:slot-properties 'foo-type-infer 'name) :type))
  (assert-eq 'string
             (getf (mopu:slot-properties 'foo-type-infer 'nickname) :type))
  (assert-eq 'integer
             (getf (mopu:slot-properties 'foo-type-infer 'age) :type))
  (assert-eq 'number
             (getf (mopu:slot-properties 'foo-type-infer 'height) :type))
  (assert-eq 'number
             (getf (mopu:slot-properties 'foo-type-infer 'width) :type))
  (assert-eq 'boolean
             (getf (mopu:slot-properties 'foo-type-infer 'lisper) :type))
  (assert (not (eq 'boolean
                   (getf (mopu:slot-properties 'foo-type-infer 'nil-is-not-bool) :type))))
  (assert-eq 'list
             (getf (mopu:slot-properties 'foo-type-infer 'empty-list) :type))
  (assert-eq 'list
             (getf (mopu:slot-properties 'foo-type-infer 'nonempty-list) :type))
  (assert-eq 'symbol
             (getf (mopu:slot-properties 'foo-type-infer 'sym) :type))
  (assert-eq 'function
             (getf (mopu:slot-properties 'foo-type-infer 'fun) :type))
  (assert-eq nil
             (getf (mopu:slot-properties 'foo-type-infer 'composite) :type)))


(define-class parent ()
  ((name nil
         :type (or null string))
   (age nil
        :type (or null number))))

(define-test inherited-type-inference ()
  (define-class child (parent)
    ((name "foo" :type string)          ; inherited.
     (age 17)                           ; inherited.
     (bio "Dummy biography")))
  (let ((c (make-instance 'child)))
    ;; Perform some assignments: CCL can catch type errors here.
    (setf (slot-value c 'age) 15)
    #+ccl
    (assert-error 'error
                  (setf (slot-value c 'name) nil))
    (assert-equal 'string
                  (getf (mopu:slot-properties 'child 'name) :type))
    ;; Given that the parent may or may not be finalized by then, we have to way
    ;; to test it but checking it falls into a reliable set of options (NIL =
    ;; parent not finalized, type = parent finalized).
    (assert-true (member (getf (mopu:slot-properties 'child 'age) :type)
                         (list nil '(or null number))
                         :test #'equal))
    (assert-true (member (getf (mopu:slot-properties 'child 'bio) :type)
                         (list nil 'string)
                         :test #'equal))))

(define-class parent2 ()
  ((name nil
         :type (or null string))
   (age nil
        :type (or null number))))
(closer-mop:ensure-finalized (find-class 'parent2))

(define-test inherited-type-inference-after-finalized ()
  (define-class child2 (parent2)
    ((name "foo" :type string)
     (age 17)
     (bio "Dummy biography"))
    (:automatic-types-p t))
  (closer-mop:ensure-finalized (find-class 'child2))
  (assert-equal '(or null number)
                (getf (mopu:slot-properties 'child2 'age) :type))
  (assert-equal 'string
                (getf (mopu:slot-properties 'child2 'name) :type))
  (assert-equal 'string
                (getf (mopu:slot-properties 'child2 'bio) :type)))
