;;;; SPDX-FileCopyrightText: dwim.hu & Atlas Engineer LLC
;;;; SPDX-License-Identifier: Public Domain

(in-package :nclasses)

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

;; setting these globally is a very bad idea, because it affects every
;; package that uses defclass-star, and what's even worse, in a
;; load-order dependent way. a recipe for bugs that are hard to debug.
(defvar *accessor-name-package* nil
  "A package, or :slot-name means the home-package of the slot-name symbol and nil means *package*")
(defvar *accessor-name-transformer* 'default-accessor-name-transformer)
(defvar *automatic-accessors-p* t)

(defvar *automatic-predicates-p* t)
(defvar *predicate-name-transformer* 'default-predicate-name-transformer
  "A function that takes the class name and its definition as argument.
Return the name of the predicate function.
The predicate function returns true when the argument is a type of the `name' class.")

(defun default-predicate-name-transformer (name &rest args)
  "Return predicate name that's suffixed with '-p' if NAME already contains a '-',
or just 'p' otherwise.."
  (declare (ignore args))
  (intern (concatenate 'string
                       (symbol-name name)
                       (string (if (position #\- (symbol-name name)) '#:-p '#:p))) ; compatibility with mutable syntax table case
          (symbol-package name)))

(defun always-dashed-predicate-name-transformer (name &rest args)
  "Return predicate name that's always suffixed with '-p'."
  (declare (ignore args))
  (intern (format nil "~a-P" name) (symbol-package name)))

(defun question-mark-predicate-name-transformer (name &rest args)
  "Return predicate name that's always suffixed with '?'."
  (declare (ignore args))
  (intern (format nil "~a?" name) (symbol-package name)))

(defun default-type-inference (initform name definition)
  "Return type of INITFORM.
This is like `type-of' but returns less specialized types for some common
subtypes, e.g.  for \"\" return 'string instead of `(SIMPLE-ARRAY CHARACTER
\(0))'.

Note that in a slot definition, '() is inferred to be a list while NIL is
inferred to be a boolean.

If the slot is found in a finalized superclass, the inferred type is then that
of the superclass.  If some superclass is not finalized, no type inference is
performed.

Non-basic form types are not inferred (returns nil).
Non-basic scalar types are derived to their own type (with `type-of')."
  (declare (ignore name definition))
  (cond
    ((and (consp initform)
          (eq (first initform) 'quote)
          (symbolp (second initform)))
     (if (eq (type-of (second initform)) 'null)
         'list                          ; The empty list.
         'symbol))
    ((and (consp initform)
          (eq (first initform) 'function))
     'function)
    ((and (consp initform)
          (not (eq (first initform) 'quote)))
     ;; Non-basic form.
     nil)
    ;; This is necessary to prevent nil-initform slots to be recognized as
    ;; boolean-typed ones. Nil does not guarantee any type.
    ((null initform)
     nil)
    (t (let* ((type (if (symbolp initform)
                        (handler-case
                            ;; We can get type of externally defined symbol.
                            (type-of (eval initform))
                          (error ()
                            ;; Don't infer type if symbol is not yet defined.
                            nil))
                        (type-of initform))))
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

;; these control whether the respective names should be exported from *package* (which is sampled at macroexpand time)
(defvar *export-class-name-p* nil)
(defvar *export-accessor-names-p* nil)
(defvar *export-slot-names-p* nil)
(defvar *export-predicate-name-p* nil)

(defvar *initarg-name-transformer* 'default-initarg-name-transformer)
(defvar *automatic-initargs-p* t)

(defvar *type-inference* 'default-type-inference
  "Function that returns the type of a slot definition (with an initform).
It takes 3 arguments:
- The initform.
- The slot name.
- The rest of the slot definition.")
(defvar *automatic-types-p* nil)

(defvar *slot-definition-transformer* 'default-slot-definition-transformer)

(defun default-slot-definition-transformer (slot-def)
  "Converts illegal (list foo) :type declarations into simple list declarations."
  (let ((name (pop slot-def))
        (type (getf slot-def :type)))
    (when (and type (listp type) (eq (first type) 'list))
      (setf (getf slot-def :type) 'list))
    (push name slot-def)
    slot-def))

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
  "Return an accessor name following the slot name NAME."
  (declare (ignore definition))
  name)

(defun dwim-accessor-name-transformer (name definition)
  "Return an accessor name using the slot name NAME suffixed with \"-OF\".
If the slot is a boolean not ending with a question mark, it is suffixed with \"-P\" instead.
If the slot ends with a question mark, the name is taken as is."
  ;; This is `hu.dwim.defclass-star:default-accessor-name-transformer'.
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

(defun question-mark-accessor-name-transformer (name definition)
  "Return an accessor name using the slot name NAME suffixed with \"-OF\".
If the slot is a boolean, it ensures the name is suffixed with \"?\"."
  ;; This is `hu.dwim.defclass-star:dwim-accessor-name-transformer'.
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

(defun slot-type-maybe-inherited (initform slot-name definition superclasses)
  (let ((all-parents-finalized-p
          (and
           (every (lambda (s) (find-class s nil)) superclasses)
           (every #'closer-mop:class-finalized-p
                  (mapcar (lambda (s) (find-class s nil)) superclasses)))))
    (cond
      ((and all-parents-finalized-p
            (find slot-name (apply #'append (mapcar #'mopu:slot-names superclasses))))
       (let ((parent (find (list slot-name) superclasses
                           :key #'mopu:slot-names :test #'intersection)))
         (when parent
           (getf (mopu:slot-properties parent slot-name) :type))))
      (all-parents-finalized-p
       (funcall *type-inference* initform slot-name definition))
      (t nil))))

(defun process-slot-definition (definition &key superclasses)
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
            (slot-name-warning-triggered? nil))
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
                   (setf slot-name-warning-triggered? t)
                   #+nil ;; this generates too many warnings which makes it kinda pointless
                   (style-warn "define-class for a slot name ~A while its home package is not *package* (~A). Default generated names will be interned into *package*!"
                               (fully-qualified-symbol-name name) *package*))))
          (prog1
              (funcall *slot-definition-transformer*
                       (append (list name)
                               (unless (eq initform 'missing)
                                 (list :initform initform))
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
                               (if (eq type 'missing)
                                   (when *automatic-types-p*
                                     (let ((type (slot-type-maybe-inherited
                                                  initform name entire-definition superclasses)))
                                       (when type
                                         (list :type type))))
                                   (when type
                                     (list :type type)))

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
         :automatic-predicates-p *automatic-predicates-p*
         :predicate-name-transformer *predicate-name-transformer*
         :export-predicate-name-p *export-predicate-name-p*
         :automatic-types-p *automatic-types-p*
         :type-inference *type-inference*
         :slot-definition-transformer *slot-definition-transformer*)))
    (values binding-names binding-values (nreverse clean-options))))

(defun build-defclass-like-expansion (name supers slots options expansion-builder)
  #+nil ;; this generates warnings where defclass would not, delme eventually?
  (unless (eq (symbol-package name) *package*)
    (style-warn "define-class for ~A while its home package is not *package* (~A)"
                (fully-qualified-symbol-name name) *package*))
  (let ((*accessor-names* nil)
        (*slot-names* nil)
        (*symbols-to-export* nil))
    (multiple-value-bind (binding-names binding-values clean-options)
        (extract-options-into-bindings options)
      (progv binding-names (mapcar #'eval binding-values)
        (let ((result (funcall expansion-builder
                               (mapcar (lambda (definition)
                                         (process-slot-definition
                                          definition
                                          :superclasses supers))
                                       slots)
                               clean-options)))
          (if (or *symbols-to-export*
                  *export-class-name-p*
                  (and *automatic-predicates-p*
                       *predicate-name-transformer*))
              `(progn
                 ,result
                 ,@(when (and *automatic-predicates-p*
                              *predicate-name-transformer*)
                     (let ((pred-name (funcall *predicate-name-transformer* name)))
                       `((unless (fboundp ',pred-name)
                           (defun ,pred-name (object)
                             #+sbcl
                             (declare (sb-ext:muffle-conditions style-warning))
                             (typep object ',name)))
                         ;; TODO shouldn't *export-predicate-name-p*
                         ;; get its default from the "e" flag? (def
                         ;; (class e) foo-bar ...) doesn't export
                         ;; FOO-BAR-P
                         ,@(when *export-predicate-name-p*
                             `((eval-when (:compile-toplevel :load-toplevel :execute)
                                 (export ',pred-name
                                         ,(package-name *package*))))))))
                 ,@(when (or *symbols-to-export*
                             *export-class-name-p*)
                     ;; Don't try to export symbols that don't belong to *package*.
                     ;; This can happen when inheriting from a class and
                     ;; overriding some slot.
                     (let ((syms (remove-if (lambda (sym)
                                              (not (eq (symbol-package sym) *package*)))
                                            (append (when *export-class-name-p*
                                                      (list name))
                                                    *symbols-to-export*))))
                       (when syms
                         `((eval-when (:compile-toplevel :load-toplevel :execute)
                             (export ',syms
                                     ,(package-name *package*)))))))
                 (find-class ',name nil))
              result))))))

(defmacro define-class (name supers slots &rest options)
  "Convenience wrapper of `defclass'.

It automates a lot of boilerplate (like exporting all the accessors of a class)
plus adds some new features.



Slot options:

- The initform can be specified as the second value.

- `:accessor', `:reader' and `:writer' can be set to either a name, NIL or T.
  If NIL, the method is not generated.
  If T, the method is generated using the `:accessor-name-transformer' class option value.

- The `:export' option allows to control the slot-name export on a per-slot basis.



New class options (defaults are NIL unless specified):

- `:slot-definition-transformer' (default: `default-slot-definition-transformer').
  A function that takes a SLOT-DEFINITION and returns a new one.

- `:accessor-name-package': The package of the generated accessors.  See
  `:accessor-name-transformer'.  Some special values are accepted:
  - `:slot-name': The package of the slot-name.
  - `:default': The current package.

- `:accessor-name-transformer' (default `default-accessor-name-transformer').

  A function of (SLOT-NAME SLOT-DEFINITION) arguments that returns the accessor
  name (a symbol).  Other transformer: `dwim-accessor-name-transformer'.

- `:automatic-accessors-p': Whether to generate accessor names automatically for
  all slots.

- `:initarg-name-transformer' (default: `default-initarg-name-transformer'.)

  A function of (SLOT-NAME SLOT-DEFINITION) arguments that returns the accessor
  name (a symbol.

- `:automatic-initargs-p' (default: T): Whether to generate accessor names
  automatically for all slots.

- `:predicate-name-transformer' (default: `default-predicate-name-transformer').

  A function of (SLOT-NAME SLOT-DEFINITION) arguments that returns the predicate
  name (a symbol).

- `:automatic-predicates-p' (default: T): Whether to generate a predicate
  for the class.

- `:type-inference' (default: `default-type-inference').

  Function that returns the type of a slot definition (with an initform).  It
  takes 3 arguments:

  - The initform.
  - The slot name.
  - The rest of the slot definition.

- `:automatic-types-p': Whether to generate the type automatically for all
  slots.  The type can still be specified manually with the `:type' slot option.

- Self-explanatory export options (all NIL by default):
  - `:export-class-name-p'
  - `:export-accessor-names-p'
  - `:export-slot-names-p'
  - `:export-predicate-name'"
  (build-defclass-like-expansion
   name supers slots options
   (lambda (processed-slots clean-options)
     `(defclass ,name ,supers
        ,processed-slots
        ,@clean-options))))

(defmacro define-condition* (name supers slots &rest options)
  "To `define-condition' what `define-class' is to `defclass'.
See `define-class' for more details."
  (build-defclass-like-expansion
   name supers slots options
   (lambda (processed-slots clean-options)
     `(define-condition ,name ,supers
        ,processed-slots
        ,@clean-options))))
