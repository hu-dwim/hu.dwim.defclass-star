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

(in-package :cl-user)

;;; try to load asdf-system-connections
(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((try (system)
           (unless (asdf:find-system system nil)
             (warn "Trying to install required dependency: ~S" system)
             (when (find-package :asdf-install)
               (funcall (read-from-string "asdf-install:install") system))
             (unless (asdf:find-system system nil)
               (error "The ~A system requires ~A." (or *compile-file-pathname* *load-pathname*) system)))
           (asdf:operate 'asdf:load-op system)))
    (try :asdf-system-connections)))

(defpackage #:defclass-star.system
    (:use :cl :asdf :asdf-system-connections))

(in-package #:defclass-star.system)

(defsystem :defclass-star
  :version "0.1"
  :author ("Attila Lendvai <attila.lendvai@gmail.com>")
  :maintainer ("Attila Lendvai <attila.lendvai@gmail.com>")
  :licence "Public Domain / BSD"
  :description "defclass* and friends to simplify life."
  :components
  ((:file "package")
   (:file "duplicates" :depends-on ("package"))
   (:file "defclass-star" :depends-on ("duplicates"))
   (:static-file "test.lisp")))

(defsystem #:defclass-star.test
  :description "Tests for the defclass-star system."
  :depends-on (:defclass-star :stefil)
  :components
  ((:file "test")))

(defmethod perform ((op test-op) (system (eql (find-system :defclass-star))))
  (operate 'load-op '#:defclass-star.test)
  (in-package #:defclass-star.test)
  (declaim (optimize (debug 3)))
  (warn "(declaim (optimize (debug 3))) was issued to help later C-c C-c'ing")
  (eval (read-from-string "(stefil:funcall-test-with-feedback-message 'test)"))
  (values))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :defclass-star))))
  nil)

(defsystem-connection defclass-star-and-swank
  :requires (:defclass-star :swank)
  :components ((:module :integration
                        :components ((:file "swank-integration")))))

(defsystem-connection defclass-star-and-contextl
  :requires (:defclass-star :contextl)
  :components ((:module :integration
                        :components ((:file "contextl-integration")))))

(defsystem-connection defclass-star-and-ucw
  :requires (:defclass-star :ucw)
  :components ((:module :integration
                        :components ((:file "ucw-integration")))))

(defsystem-connection defclass-star-and-computed-class
  :requires (:defclass-star :computed-class)
  :components ((:module :integration
                        :components ((:file "computed-class-integration")))))

(defsystem-connection defclass-star-and-cl-def
  :requires (:defclass-star :cl-def)
  :components ((:module :integration
                        :components ((:file "cl-def-integration")))))

(defsystem-connection defclass-star-and-cl-def-and-contextl
  :requires (defclass-star-and-cl-def defclass-star-and-contextl)
  :components ((:module :integration
                        :components ((:file "cl-def-and-contextl-integration")))))
