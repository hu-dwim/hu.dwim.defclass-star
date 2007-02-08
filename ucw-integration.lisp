;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :defclass-star)

(def-star-macro defcomponent* ucw:defcomponent)
(export 'defcomponent* :defclass-star)
(import 'defcomponent* :ucw)
(export 'defcomponent* :ucw)

