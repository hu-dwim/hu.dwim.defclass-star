;;; -*- encoding: utf-8 -*-
;;;
;;; Copyright (c) 2021 by the authors.
;;;
;;; See LICENCE for details.

(require 'lisp-mode)

;; usage example: add something like this to your init.el:
;;
;; (add-to-list 'load-path (expand-file-name "~/common-lisp/hu.dwim.defclass-star/emacs/"))
;; (require 'hu.dwim.defclass-star)

(defun hu.dwim.defclass-star:lisp-mode-hook ()
  (let ((overrides
         '((defclass* defclass)
           (defcondition* defcondition))))
    (dolist (el overrides)
      (put (cl-first el) 'common-lisp-indent-function
           (if (symbolp (cl-second el))
               (get (cl-second el) 'common-lisp-indent-function)
               (cl-second el))))))

(add-hook 'lisp-mode-hook 'hu.dwim.defclass-star:lisp-mode-hook)

(provide 'hu.dwim.defclass-star)
