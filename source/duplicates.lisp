;;;; SPDX-FileCopyrightText: hu.dwim & Atlas Engineer LLC
;;;; SPDX-License-Identifier: Public Domain

(in-package :nclass)

;;; The content of this file is copied over from some other libraries to
;;; decrease the number of dependencies.

(defun concatenate-symbol (&rest args)
  "Args are processed as parts of the result symbol with two exceptions except when a package is encountered then it is stored as the target package at intern."
  (let* ((package nil)
         (symbol-name (string-upcase
                       (with-output-to-string (str)
                         (dolist (arg args)
                           (typecase arg
                             (string (write-string arg str))
                             (package (setf package arg))
                             (symbol (unless package
                                       (setf package (symbol-package arg)))
                                     (write-string (symbol-name arg) str))
                             (integer (write-string (princ-to-string arg) str))
                             (character (write-char arg) str)
                             (t (error "Cannot convert argument ~S to symbol" arg))))))))
    (if package
        (intern symbol-name package)
        (intern symbol-name))))

(defun remove-keywords (plist &rest keywords)
  "Creates a copy of PLIST without the listed KEYWORDS."
  (declare (optimize (speed 3)))
  (loop for cell = plist :then (cddr cell)
        for el = (car cell)
        while cell
        unless (member el keywords :test #'eq)
        collect el
        and collect (cadr cell)
        and do (assert (cdr cell) () "Not a proper plist")))

(define-modify-macro remf-keywords (&rest keywords) remove-keywords
  "Removes the properties identified by KEYWORDS from PLACE.")

;; from hu.dwim.util
(defun fully-qualified-symbol-name (symbol &key separator)
  (let* ((symbol-name (symbol-name symbol))
         (package (symbol-package symbol))
         (keyword-package (load-time-value (find-package "KEYWORD"))))
    (concatenate 'string
                 (unless (eq package keyword-package)
                   (package-name package))
                 (or separator
                     (if (or (not (eq package keyword-package))
                             (not (eq (nth-value 1 (find-symbol symbol-name package)) :external)))
                         "::"
                         ":"))
                 symbol-name)))
