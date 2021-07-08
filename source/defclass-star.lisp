;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.defclass-star)

(enable-sharp-boolean-syntax)

(defmacro make-name-transformer (&rest elements)
  "Return an accessor name transformer.
The unquoted `name' symbol argument is substituted for the slot name.
Class option examples:

  :accessor-name-transformer (make-name-transformer \"FOO-\" name \"-BAR\")

Use the slot name directly:

  :accessor-name-transformer (make-name-transformer name)"
  `(lambda (name definition)
    (declare (ignorable definition))
    (concatenate-symbol ,@(mapcar (lambda (el)
                                    (if (and (symbolp el)
                                             (string= (symbol-name el) "NAME"))
                                        'name
                                        el))
                                  elements))))

;; more or less public vars (it's discouraged to set them globally)
(defvar *accessor-name-package* nil
  "A package, or :slot-name means the home-package of the slot-name symbol and nil means *package*")
(defvar *accessor-name-transformer* 'default-accessor-name-transformer)
(defvar *automatic-accessors-p* #t)

(defvar *predicate-name-transformer* 'default-predicate-name-transformer
  "A function that takes the class name and its definition as argument.
Return the predicate name.
The predicate returns non-nil when the argument is of the `name' class.")

(defun default-predicate-name-transformer (name &rest args)
  (declare (ignore args))
  (intern (concatenate 'string
                       (symbol-name name)
                       (string (if (position #\- (symbol-name name)) '#:-p '#:p))) ; compatibility with mutable syntax table case
          (symbol-package name)))

;; these control whether the respective names should be exported from *package* (which is sampled at macroexpand time)
(defvar *export-class-name-p* nil)
(defvar *export-accessor-names-p* nil)
(defvar *export-slot-names-p* nil)
(defvar *export-predicate-name-p* nil)

(defvar *initarg-name-transformer* 'default-initarg-name-transformer)
(defvar *automatic-initargs-p* #t)

(defvar *slot-definition-transformer* 'default-slot-definition-transformer)

(defun default-slot-definition-transformer (slot-def)
  "Converts illegal (list foo) :type declarations into simple list declarations."
  (let ((name (pop slot-def))
        (type (getf slot-def :type)))
    (when (and type (listp type) (eq (first type) 'list))
      (setf (getf slot-def :type) 'list))
    (push name slot-def)
    slot-def))

(defvar *type-inference* nil
  "Fallback type inference function.
See `basic-type-inference' for a possible value.
Set this to nil to disable inference.")

(defun basic-type-inference (value)
  "Return general type of VALUE.
This is like `type-of' but returns less specialized types for some common
subtypes, e.g.  for \"\" return 'string instead of `(SIMPLE-ARRAY CHARACTER
\(0))'.

Note that in a slot definition, '() is inferred to be a list while NIL is
inferred to be a boolean.

Non-basic form types are not inferred (returns nil).
Non-basic scalar types are derived to their own type (with `type-of')."
  (cond
    ((and (consp value)
          (eq (first value) 'quote)
          (symbolp (second value)))
     (if (eq (type-of (second value)) 'null)
         'list                    ; The empty list.
         'symbol))
    ((and (consp value)
          (eq (first value) 'function))
     'function)
    ((and (consp value)
          (not (eq (first value) 'quote)))
     ;; Non-basic form.
     nil)
    (t (let* ((type (if (symbolp value)
                        (handler-case
                            ;; We can get type of externally defined symbol.
                            (type-of (eval value))
                          (error ()
                            ;; Don't infer type if symbol is not yet defined.
                            nil))
                        (type-of value))))
         (if type
             (flet ((derive-type (general-type)
                      (when (subtypep type general-type)
                        general-type)))
               (or (some #'derive-type '(string boolean list array hash-table integer
                                         complex number))
                   ;; Only allow objects of the same type by default.
                   ;; We could have returned nil to inhibit the generation of a
                   ;; :type property.
                   type))
             nil)))))

(defvar *allowed-slot-definition-properties* '(:documentation :type :reader :writer :allocation :export)
  "Holds a list of keywords that are allowed in slot definitions (:accessor and :initarg are implicitly included).")

;; expand-time temporary dynamic vars
(defvar *accessor-names*)
(defvar *slot-names*)
(defvar *symbols-to-export*)

(define-condition hu.dwim.defclass-star-style-warning (simple-condition style-warning)
  ())

(defun style-warn (datum &rest args)
  (warn 'hu.dwim.defclass-star-style-warning :format-control datum :format-arguments args))

(defun slot-name-package (name)
  (if (packagep *accessor-name-package*)
      *accessor-name-package*
      (case *accessor-name-package*
        (:slot-name (symbol-package name))
        (:default *package*)
        (t *package*))))

(defun default-accessor-name-transformer (name definition)
  (let* ((type (getf definition :type))
         (package (slot-name-package name))
         (name-string (string name))
         (last-char (elt name-string (1- (length name-string))))
         (ends-with-question-mark? (char= last-char #\?)))
    (cond
      ((and (eq type 'boolean)
            (not ends-with-question-mark?))
       (cond ((char-equal last-char #\p)
              name)
             ;; i like unconditional '-p' postfix. ymmv.
             #+nil((not (find #\- name-string))
                   (concatenate-symbol name '#:p package))
             (t (concatenate-symbol name '#:-p package))))
      (ends-with-question-mark?
       name)
      (t (concatenate-symbol name '#:-of package)))))

(defun dwim-accessor-name-transformer (name definition)
  (let* ((type (getf definition :type))
         (package (slot-name-package name))
         (name-string (string name))
         (last-char (elt name-string (1- (length name-string)))))
    (if (eq type 'boolean)
        (if (char= last-char #\?)
            name
            (concatenate-symbol name "?" package))
        (concatenate-symbol name '#:-of package))))

(defun default-initarg-name-transformer (name definition)
  (declare (ignorable definition))
  (concatenate-symbol name #.(symbol-package :asdf)))

(defun process-slot-definition (definition)
  (unless (consp definition)
    (setf definition (list definition)))
  (let ((name (pop definition))
        (initform 'missing)
        (entire-definition definition))
    (assert name)
    (push name *slot-names*)
    (if (oddp (length definition))
        (progn
          (setf initform (pop definition))
          (setf entire-definition definition)
          (when (eq initform :unbound)
            (setf initform 'missing)))
        (setf initform (getf definition :initform 'missing)))
    (assert (every #'keywordp (loop for el :in definition :by #'cddr
                                    collect el))
            () "Found non-keywords in ~S" definition)
    (destructuring-bind (&key (accessor 'missing) (initarg 'missing)
                              (reader 'missing) (writer 'missing)
                              (export 'missing) (type 'missing)
                              &allow-other-keys)
        definition
      (remf-keywords definition :accessor :reader :writer :initform :initarg :export :type)
      (let ((unknown-keywords (loop for el :in definition :by #'cddr
                                    unless (or (member t *allowed-slot-definition-properties*)
                                               (member el *allowed-slot-definition-properties*))
                                    collect el))
            (slot-name-warning-triggered? #f))
        (when unknown-keywords
          (style-warn "Unexpected properties in slot definition ~S.~%~
                       The unexpected properties are ~S.~%~
                       To avoid this warning (pushnew (or T :your-custom-keyword) hu.dwim.defclass-star:*allowed-slot-definition-properties*)"
                      entire-definition unknown-keywords))
        (flet ((provided-p (value)
                 (and value
                      (not (eq value 'missing))))
               (transform-accessor ()
                 (funcall *accessor-name-transformer* name entire-definition))
               (maybe-warn-for-slot-name ()
                 (unless (or slot-name-warning-triggered?
                             (eq (symbol-package name) *package*))
                   (setf slot-name-warning-triggered? #t)
                   #+nil ;; this generates too many warnings which makes it kinda pointless
                   (style-warn "defclass* for a slot name ~A while its home package is not *package* (~A). Default generated names will be interned into *package*!"
                               (fully-qualified-symbol-name name) *package*))))
          (prog1
              (funcall *slot-definition-transformer*
                       (append (list name)
                               (unless (eq initform 'missing)
                                 (append (list :initform initform)
                                         (cond
                                           ((and type (not (eq type 'missing)))
                                            (list :type type))
                                           (*type-inference*
                                            (let ((initform-type (funcall *type-inference* initform)))
                                              (when initform-type
                                                (list :type initform-type)))))))
                               (if (and (eq accessor 'missing)
                                        (eq reader 'missing)
                                        (eq writer 'missing))
                                   (when *automatic-accessors-p*
                                     (maybe-warn-for-slot-name)
                                     (setf accessor (transform-accessor))
                                     (list :accessor accessor))
                                   (let ((transformed-accessor (transform-accessor)))
                                     (append (progn
                                               (when (eq accessor t)
                                                 (setf accessor transformed-accessor))
                                               (when (provided-p accessor)
                                                 (list :accessor accessor)))
                                             (progn
                                               (when (eq reader t)
                                                 (setf reader transformed-accessor))
                                               (when (provided-p reader)
                                                 (list :reader reader)))
                                             (progn
                                               (when (eq writer t)
                                                 (setf writer `(setf ,transformed-accessor)))
                                               (when (provided-p writer)
                                                 (list :writer writer))))))
                               (if (eq initarg 'missing)
                                   (when *automatic-initargs-p*
                                     (list :initarg (funcall *initarg-name-transformer* name entire-definition)))
                                   (when initarg
                                     (list :initarg initarg)))
                               definition))
            (when (provided-p accessor)
              (pushnew accessor *accessor-names*))
            (when (provided-p reader)
              (pushnew reader *accessor-names*))
            (when (provided-p writer)
              (pushnew (second writer) *accessor-names*))
            (if (not (eq export 'missing))
                (ecase export
                  (:accessor
                   (when accessor
                     (push accessor *symbols-to-export*)))
                  ((:slot :name :slot-name)
                   (push name *symbols-to-export*))
                  ((t)
                   (when (and accessor (not (eq accessor 'missing)))
                     (push accessor *symbols-to-export*))
                   (push name *symbols-to-export*))
                  ((nil)))
                (progn
                  (when *export-accessor-names-p*
                    (when (and accessor (not (eq accessor 'missing)))
                      (push accessor *symbols-to-export*)))
                  (when *export-slot-names-p*
                    (push name *symbols-to-export*))))))))))

(defun extract-options-into-bindings (options)
  (let ((binding-names)
        (binding-values)
        (clean-options))
    (macrolet ((rebinding-table (&rest args)
                 `(case (car option)
                   ,@(loop for (arg-name var-name) :on args :by #'cddr
                           collect `(,arg-name
                                     (assert (= (length option) 2))
                                     (push ',var-name binding-names)
                                     (push (second option) binding-values)))
                   (t (push option clean-options)))))
      (dolist (option options)
        (rebinding-table
         :accessor-name-package *accessor-name-package*
         :accessor-name-transformer *accessor-name-transformer*
         :automatic-accessors-p *automatic-accessors-p*
         :initarg-name-transformer *initarg-name-transformer*
         :automatic-initargs-p *automatic-initargs-p*
         :export-class-name-p *export-class-name-p*
         :export-accessor-names-p *export-accessor-names-p*
         :export-slot-names-p *export-slot-names-p*
         :slot-definition-transformer *slot-definition-transformer*
         :type-inference *type-inference*)))
    (values binding-names binding-values (nreverse clean-options))))

(defun build-defclass-like-expansion (name supers slots options expansion-builder
                                      &key
                                      (export-class-name *export-class-name-p*)
                                      (export-accessor-names *export-accessor-names-p*)
                                      (export-slot-names *export-slot-names-p*)
                                      (export-predicate-name *export-predicate-name-p*)
                                      (predicate-name-transformer *predicate-name-transformer*))
  (declare (ignore supers))
  #+nil ;; this generates warnings where defclass would not, delme eventually?
  (unless (eq (symbol-package name) *package*)
    (style-warn "defclass* for ~A while its home package is not *package* (~A)"
                (fully-qualified-symbol-name name) *package*))
  (let ((*accessor-names* nil)
        (*slot-names* nil)
        (*symbols-to-export* nil)
        (*export-class-name-p* export-class-name)
        (*export-accessor-names-p* export-accessor-names)
        (*export-slot-names-p* export-slot-names)
        (*export-predicate-name-p* export-predicate-name)
        (*predicate-name-transformer* predicate-name-transformer))
    (multiple-value-bind (binding-names binding-values clean-options)
        (extract-options-into-bindings options)
      (progv binding-names (mapcar #'eval binding-values)
        (let ((result (funcall expansion-builder
                               (mapcar 'process-slot-definition slots)
                               clean-options)))
          (if (or *symbols-to-export*
                  *export-class-name-p*
                  *predicate-name-transformer*)
              `(progn
                 ,result
                 ,@(when *predicate-name-transformer*
                     (let ((pred-name (funcall *predicate-name-transformer* name)))
                       `((defun ,pred-name (object)
                           (typep object ',name))
                         ,@(when *export-predicate-name-p*
                             `((eval-when (:compile-toplevel :load-toplevel :execute)
                                 (export ',pred-name
                                         ,(package-name *package*))))))))
                 ,@(when (or *symbols-to-export*
                             *export-class-name-p*)
                     `((eval-when (:compile-toplevel :load-toplevel :execute)
                         ;; Don't try to export symbols that don't belong to *package*.
                         ;; This can happen when inheriting from a class and
                         ;; overriding some slot.
                         (export '(,@(remove-if (lambda (sym)
                                                  (not (eq (symbol-package sym) *package*)))
                                      (append (when *export-class-name-p*
                                                (list name))
                                       *symbols-to-export*)))
                                 ,(package-name *package*)))
                       (find-class ',name nil))))
            result))))))

(defmacro defclass* (name supers slots &rest options)
  (build-defclass-like-expansion
   name supers slots options
   (lambda (processed-slots clean-options)
     `(defclass ,name ,supers
        ,processed-slots
        ,@clean-options))))

(defmacro defcondition* (name supers slots &rest options)
  (build-defclass-like-expansion
   name supers slots options
   (lambda (processed-slots clean-options)
     `(define-condition ,name ,supers
        ,processed-slots
        ,@clean-options))))
