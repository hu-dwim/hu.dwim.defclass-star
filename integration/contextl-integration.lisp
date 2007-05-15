;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :defclass-star)

(def-star-macro deflayer* contextl:deflayer)

(integrated-export 'deflayer* :contextl)

(pushnew :special *allowed-slot-definition-properties*)
