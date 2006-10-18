;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy 
;;; of this software and associated documentation files (the "Software"), to deal 
;;; in the Software without restriction, including without limitation the rights 
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
;;; copies of the Software, and to permit persons to whom the Software is furnished 
;;; to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in 
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
;;; IN THE SOFTWARE.

(in-package #:defclass-star.system)

(defpackage #:defclass-star.test
  (:use :cl :defclass-star :5am))

(defpackage #:defclass-star.test-dummy
  (:use :cl :defclass-star :5am))

(in-package #:defclass-star.test)

(def-suite :defclass-star :description "defclass* tests")

(in-suite :defclass-star)

(defmacro exp= (macro result)
  `(is (equal (macroexpand-1 ',macro)
              ',result)))

(defmacro exp=* (macro result)
  `(is (equal (macroexpand-1 ',macro)
              ',(eval result))))

(defmacro slot= (slotd result)
  `(is (equal (let ((defclass-star::*accessor-names* nil)
                    (defclass-star::*slot-names* nil))
                (defclass-star::process-slot-definition ',slotd))
             ',result)))

(defmacro test* (name &body body)
  `(test ,name
    (let ((*automatic-accessors-p* t)
          (*accessor-name-transformer* 'default-accessor-name-transformer)
          (*automatic-initargs-p* t)
          (*initarg-name-transformer* 'default-initarg-name-transformer)
          (defclass-star::*export-class-name-p* nil)
          (defclass-star::*export-accessor-names-p* nil)
          (defclass-star::*export-slot-names-p* nil))
      ,@body)))

(test* nop
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

(test* accessors
  (slot= (slot1 :unbound :accessor slot1-custom :initarg slot1-custom)
         (slot1 :accessor slot1-custom :initarg slot1-custom))
  (slot= (slot1)
         (slot1 :accessor slot1-of :initarg :slot1))
  (slot= (slot1 :unbound :type boolean)
         (slot1 :accessor slot1p :initarg :slot1 :type boolean))
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

(test* reconfiguration
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
          (:accessor-name-package (find-package '#:defclass-star.test-dummy)))
        (defclass some-class (some super classes)
          ((slot1 :accessor defclass-star.test-dummy::slot1-of :initarg :slot1))))
  (signals error (macroexpand-1 '(defclass* some-class (some super classes)
                                  ((slot1))
                                  (:accessor-name-transformer too many)))))

(test* full
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
           (slot2 :unbound :accessor slot2-custom))
          (:export-accessor-names-p t)
          (:export-class-name-p t)
          (:export-slot-names-p t))
        `(progn
          (defclass some-class (some super classes)
            ((slot1 :initform 42 :accessor slot1-of :initarg :slot1 :documentation "zork")
             (slot2 :accessor slot2-custom :initarg :slot2)))
          (export (list 'some-class 'slot1-of 'slot2-custom 'slot1 'slot2) ,*package*))))
