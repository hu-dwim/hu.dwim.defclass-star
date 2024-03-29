;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.defclass-star)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import
   '(*export-class-name-p* *export-slot-names-p* *export-accessor-names-p*)
   (find-package :hu.dwim.defclass-star/test)))

(in-package :hu.dwim.defclass-star/test)

(defsuite* (test :in root-suite) ()
  ;; TODO FIXME? this only works when running the entire test
  ;; suite. or just update the tests with the expansions of the
  ;; predicates?
  (let ((hu.dwim.defclass-star::*predicate-name-transformer* nil))
    (-run-child-tests-)))

(defmacro exp= (macro result)
  `(is (equal (macroexpand-1 ',macro)
              ',result)))

(defmacro exp=* (macro result)
  `(is (equal (macroexpand-1 ',macro)
              ',(eval result))))

(defmacro exp-signals (condition-spec class-definitions)
  `(signals ,condition-spec
    (macroexpand-1 ',class-definitions)))

(defmacro exp-warns (class-definitions)
  `(exp-signals warning ,class-definitions))

(defmacro exp-errors (class-definitions)
  `(exp-signals error ,class-definitions))

(defmacro slot= (slotd result &rest head)
  (unless head
    (setf head (list 'is)))
  `(,@head (equal (let ((hu.dwim.defclass-star::*accessor-names* nil)
                        (hu.dwim.defclass-star::*slot-names* nil))
                    (hu.dwim.defclass-star::process-slot-definition ',slotd))
            ',result)))

(defmacro slot-signals (condition-spec slotd)
  `(signals ,condition-spec
    (let ((hu.dwim.defclass-star::*accessor-names* nil)
          (hu.dwim.defclass-star::*slot-names* nil))
      (hu.dwim.defclass-star::process-slot-definition ',slotd))))

(defmacro slot-warns (slotd)
  `(slot-signals warning ,slotd))

(defmacro slot-errors (slotd)
  `(slot-signals error ,slotd))

(defmacro deftest* (name args &body body)
  `(deftest ,name ,args
    (let ((*automatic-accessors-p* t)
          (*accessor-name-transformer* 'default-accessor-name-transformer)
          (*automatic-initargs-p* t)
          (*initarg-name-transformer* 'default-initarg-name-transformer)
          (*export-class-name-p* nil)
          (*export-accessor-names-p* nil)
          (*export-slot-names-p* nil))
      ,@body)))

(deftest* nop ()
  (exp= (defclass* some-class (some super classes)
          ()
          (1 2)
          (3 4))
        (defclass some-class (some super classes)
          ()
          (1 2)
          (3 4)))

  (exp= (defclass* some-class (some super classes)
          ((slot1 :unbound :accessor foo :initarg :bar))
          (1 2)
          (3 4))
        (defclass some-class (some super classes)
          ((slot1 :accessor foo :initarg :bar))
          (1 2)
          (3 4))))

(deftest* accessors ()
  (slot= (slot1 :unbound :accessor slot1-custom :initarg slot1-custom)
         (slot1 :accessor slot1-custom :initarg slot1-custom))
  (slot= (slot1)
         (slot1 :accessor slot1-of :initarg :slot1))
  (slot= (slot1 :unbound :type boolean)
         (slot1 :accessor slot1-p :initarg :slot1 :type boolean))
  (slot= (slotp :unbound :type boolean)
         (slotp :accessor slotp :initarg :slotp :type boolean))
  (slot= (slot-name :unbound :type boolean)
         (slot-name :accessor slot-name-p :initarg :slot-name :type boolean))
  (let ((*automatic-accessors-p* nil)
        (*automatic-initargs-p* nil))
    (slot= (slot1)
           (slot1)))
  (let ((*accessor-name-transformer* (make-name-transformer "FOO-" name "-BAR"))
        (*initarg-name-transformer* (make-name-transformer "BAZ-" name)))
    (slot= (slot1)
           (slot1 :accessor foo-slot1-bar :initarg baz-slot1))))

(deftest* reconfiguration ()
  (exp= (defclass* some-class (some super classes)
          ((slot1))
          (1 2)
          (:accessor-name-transformer (make-name-transformer name "-ZORK"))
          (3 4))
        (defclass some-class (some super classes)
          ((slot1 :accessor slot1-zork :initarg :slot1))
          (1 2)
          (3 4)))
  (exp= (defclass* some-class (some super classes)
          ((slot1))
          (:accessor-name-package (find-package :hu.dwim.defclass-star/test.dummy)))
        (defclass some-class (some super classes)
          ((slot1 :accessor hu.dwim.defclass-star/test.dummy::slot1-of :initarg :slot1)))))

(deftest* full ()
  (exp= (defclass* some-class (some super classes)
          ((slot1 :unbound :documentation "zork"))
          (1 2)
          (3 4))
        (defclass some-class (some super classes)
          ((slot1 :accessor slot1-of :initarg :slot1 :documentation "zork"))
          (1 2)
          (3 4)))
  (exp= (defclass* some-class (some super classes)
          ((slot1 42 :documentation "zork"))
          (1 2)
          (:automatic-accessors-p nil)
          (:automatic-initargs-p nil)
          (3 4))
        (defclass some-class (some super classes)
          ((slot1 :initform 42 :documentation "zork"))
          (1 2)
          (3 4)))
  (exp=* (defclass* some-class (some super classes)
          ((slot1 42 :documentation "zork")
           (slot2 :unbound :accessor slot2-custom)
           (slot3 :unbound :accessor nil :export :accessor))
          (:export-accessor-names-p t)
          (:export-class-name-p t)
          (:export-slot-names-p t))
        `(progn
          (defclass some-class (some super classes)
            ((slot1 :initform 42 :accessor slot1-of :initarg :slot1 :documentation "zork")
             (slot2 :accessor slot2-custom :initarg :slot2)
             (slot3 :initarg :slot3)))
          (eval-when (:compile-toplevel :load-toplevel :execute)
            (export '(some-class slot2 slot2-custom slot1 slot1-of) ,(package-name *package*)))
          (find-class 'some-class nil)))
  (exp=* (defclass* some-class (some super classes)
           ((slot1 42 :documentation "zork" :export :slot)
            (slot2 :unbound :accessor slot2-custom)
            (slot3 42 :export :accessor)
            (slot4 42 :export nil))
           (:export-accessor-names-p t)
           (:export-class-name-p t)
           (:export-slot-names-p t))
         `(progn
            (defclass some-class (some super classes)
              ((slot1 :initform 42 :accessor slot1-of :initarg :slot1 :documentation "zork")
               (slot2 :accessor slot2-custom :initarg :slot2)
               (slot3 :initform 42 :accessor slot3-of :initarg :slot3)
               (slot4 :initform 42 :accessor slot4-of :initarg :slot4)))
            (eval-when (:compile-toplevel :load-toplevel :execute)
              (export '(some-class slot3-of slot2 slot2-custom slot1) ,(package-name *package*)))
            (find-class 'some-class nil))))

(deftest* warnings-and-errors ()
  (let ((*allowed-slot-definition-properties* *allowed-slot-definition-properties*))
    (push :asdf *allowed-slot-definition-properties*)
    (push :unspecified *allowed-slot-definition-properties*)
    (slot= (slot1 :asdf 42 :unspecified 43)
           (slot1 :accessor slot1-of :initarg :slot1 :asdf 42 :unspecified 43)))
  (slot-warns (slot1 :unbound :asdf slot1-custom))
  (slot-warns (slot1 :asdf slot1-custom))
  (slot-errors (slot1 :unbound foo bar))
  (slot-errors (slot1 foo bar))

  (exp-errors (defclass* some-class ()
                ((slot1))
                (:accessor-name-transformer 'default-accessor-name-transformer many)))
  #+() ;; this has been commented out...
  (exp-warns (defclass* hu.dwim.defclass-star::some-class ()
               ())))

(deftest* bug/export-slot-name-from-foreign-package ()
  (let* ((pkg1 (find-package :hu.dwim.defclass-star/test.pkg1))
         (pkg2 (find-package :hu.dwim.defclass-star/test.pkg2))
         (exp1 (let ((*package* pkg1))
                 (macroexpand-1
                  '(defclass* hu.dwim.defclass-star/test.pkg1::foo ()
                    ((hu.dwim.defclass-star/test.pkg1::foo-name))
                    (:export-slot-names-p t)))))
         (exp2 (let ((*package* pkg2))
                 (macroexpand-1
                  '(defclass* hu.dwim.defclass-star/test.pkg2::bar
                    (hu.dwim.defclass-star/test.pkg1::foo)
                    ;; override the slot in the superclass
                    ((hu.dwim.defclass-star/test.pkg1::foo-name :initform ""))
                    (:export-slot-names-p t))))))
    ;; The bug is/was:
    ;; These symbols are not accessible in the HU.DWIM.DEFCLASS-STAR/TEST.PKG2 package:
    ;;   (HU.DWIM.DEFCLASS-STAR/TEST.PKG1:FOO-NAME)
    (finishes (let ((*package* pkg1))
                (eval exp1)))
    (with-expected-failures
      ;; TODO FIXME
      (finishes (let ((*package* pkg2))
                  (eval exp2))))))
