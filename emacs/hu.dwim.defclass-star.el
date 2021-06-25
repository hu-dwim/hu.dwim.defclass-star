;;; -*- encoding: utf-8 -*-
;;;
;;; Copyright (c) 2021 by the authors.
;;;
;;; See LICENCE for details.

(require 'lisp-mode)
(provide 'hu.dwim.defclass-star)

;; usage example: add something like this to your init.el:
;;
;; (add-to-list 'load-path (expand-file-name "~/common-lisp/hu.dwim.defclass-star/emacs/"))
;; (require 'hu.dwim.defclass-star)

(defun hu.dwim.defclass-star:lisp-mode-hook ()
  (let ((overrides
         '((defclass* defclass)
           (defcondition* defcondition))))
    (dolist (el overrides)
      (put (first el) 'common-lisp-indent-function
           (if (symbolp (second el))
               (get (second el) 'common-lisp-indent-function)
               (second el))))))

(add-hook 'lisp-mode-hook 'hu.dwim.defclass-star:lisp-mode-hook)
