;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :defclass-star)

(def-star-macro defcomponent* ucw:defcomponent)

(integrated-export 'defcomponent* :ucw)

(push :component *allowed-slot-definition-properties*)
(push :backtrack *allowed-slot-definition-properties*)
