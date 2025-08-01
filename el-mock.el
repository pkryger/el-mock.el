;;; el-mock.el --- Tiny Mock and Stub framework in Emacs Lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2008, 2010, 2012  rubikitch
;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 1.25.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: lisp, testing, unittest
;; URL: http://github.com/rejeep/el-mock.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Emacs Lisp Mock is a library for mocking and stubbing using
;; readable syntax.  Most commonly Emacs Lisp Mock is used in
;; conjunction with Emacs Lisp Expectations, but it can be used in
;; other contexts.

;;; Commands:
;;
;; Below are complete command list:
;;
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;; Emacs Lisp Mock provides two scope interface of mock and stub:
;; `with-mock' and `mocklet'. `with-mock' only defines a
;; scope. `mocklet' is more sophisticated interface than `with-mock':
;; `mocklet' defines local mock and stub like `let', `flet', and
;; `macrolet'.

;; Within `with-mock' body (or argument function specified in
;; `mock-protect'), you can create a mock and a stub. To create a
;; stub, use `stub' macro. To create a mock, use `mock' macro.

;; For further information: see docstrings.
;; [EVAL IT] (describe-function 'with-mock)
;; [EVAL IT] (describe-function 'mocklet)
;; [EVAL IT] (describe-function 'stub)
;; [EVAL IT] (describe-function 'mock)

;;; Code:

(require 'cl-lib)

;; FIXME: This is kept for compatibility with .elc files compiled with old
;; versions of `el-mock'.  Is it worth keeping?
(define-obsolete-variable-alias
  '-stubbed-functions 'mock--stubbed-functions "1.26?")
(define-obsolete-variable-alias
  '-mocked-functions 'mock--mocked-functions "1.26?")

(defvar mock--stubbed-functions nil)
(defvar mock--mocked-functions nil)
(defvar mock-verify-list nil)
(defvar mock--in-mocking nil)

;;;; stub setup/teardown
(defun mock--stub-setup (funcsym function)
  "Setup FUNCTION as as stub for FUNCSYM."
  (mock-suppress-redefinition-message
   (lambda ()
     (when (fboundp funcsym)
       (put funcsym 'mock-original-func (symbol-function funcsym)))
     (cl-pushnew funcsym mock--stubbed-functions)
     (fset funcsym function))))

(defun stub/setup (funcsym value)
  ;; FIXME: This is kept for compatibility with .elc files compiled with old
  ;; versions of `el-mock'.  Is it worth keeping?
  (declare (obsolete mock--stub-setup "el-mock-1.26?"))
  (mock--stub-setup funcsym `(lambda (&rest x) ,value)))

(defun stub/teardown (funcsym)    ;FIXME: `mock-' namespace?
  ;; FIXME: Better not call this function by accident since it will
  ;; merrily undefine the function you pass it :-(
  (mock-suppress-redefinition-message
   (lambda ()
     (let ((func (get funcsym 'mock-original-func)))
       (if (not func) ;; FIXME: Not needed since Emacs≥24.4
           (fmakunbound funcsym)
         (fset funcsym func)
         ;; may be unadviced
         )))))

;;;; mock setup/teardown
(defun mock/setup (func-spec value times)
  "Setup FUNC-SPEC to be a mock returning VALUE and to be called number of TIMES."
  (let ((funcsym (car func-spec)))
    (put funcsym 'mock-call-count 0)
    (cl-pushnew funcsym mock--mocked-functions)
    (mock--stub-setup funcsym
                      `(lambda (&rest actual-args)
                         (cl-incf (get ',funcsym 'mock-call-count))
                         (add-to-list 'mock-verify-list
                                      (list ',funcsym ',(cdr func-spec) actual-args ,times))
                         ,value))))

(defun not-called/setup (funcsym)
  ;; FIXME: This is kept for compatibility with .elc files compiled with old
  ;; versions of `el-mock'.  Is it worth keeping?
  (declare (obsolete mock--stub-setup "el-mock-1.26?"))
  (mock--stub-setup funcsym (lambda (&rest _actual-args)
                             (signal 'mock-error '(called)))))

;;;; mock verify
(define-error 'mock-error "Mock error")
(defun mock-verify ()
  "Verify expectations on mocks."
  (cl-loop for f in mock--mocked-functions
           when (equal 0 (get f 'mock-call-count))
           do (signal 'mock-error (list 'not-called f)))
  (cl-loop for args in mock-verify-list
           do
           (apply #'mock-verify-args args)))

(defun mock-filter-matcher-explainers (args)
  "Remove explainers from matchers in ARGS."
  (mapcar (lambda (arg)
            (if (eq (car-safe arg) '~=)
                (list (car arg) (cadr arg))
              arg))
          args))

(defun mock-verify-arg (expected actual index funcsym expected-args actual-args)
  "Verify that INDEX'th argument's EXPECTED value is satisfied by the ACTUAL one.
If the verification fails `mock-error' is signaled with FUNCSYM and a
list of EXPECTED-ARGS and ACTUAL-ARGS."
  (unless (eq expected '*)              ; `*' is wildcard argument
    (cond
     ((eq (car-safe expected) '~=)
      (let* ((matcher (cadr expected))
             (explainer (or (caddr expected)
                            (and (symbolp matcher)
                                 (or
                                  (function-get matcher 'mock-explainer)
                                  (function-get matcher 'ert-explainer))))))
        (unless (funcall matcher actual)
          (signal 'mock-error (append (list (cons funcsym (mock-filter-matcher-explainers expected-args))
                                            (cons funcsym actual-args)
                                            :arg-index index
                                            :failing-matcher matcher
                                            :failing-arg actual)
                                      (when explainer
                                        (list :explanation (funcall explainer actual))))))))
     ((let ((expected (eval expected)))
        (unless (equal expected actual)
          (signal 'mock-error (list (cons funcsym (mock-filter-matcher-explainers expected-args))
                                    (cons funcsym actual-args)
                                    :arg-index index
                                    :expected-arg expected
                                    :actual-arg actual))))))))


(defun mock-verify-args (funcsym expected-args actual-args expected-times)
  "Verify that EXPECTED-ARGS are satisfied by ACTUAL-ARGS.
Also verify that the FUNCSYM has been called EXPECTED-TIMES.  If
verification fails `mock-error' is signaled."
  (let ((at-least (cl-position '** expected-args))
        (actual (length actual-args))
        (expected (length expected-args)))
    (unless (if at-least
                (<= at-least actual)
              (= expected actual))
      (signal 'mock-error (append
                           (list (cons funcsym expected-args)
                                 (cons funcsym actual-args)
                                 :expected-args-number)
                           (if at-least
                               (list 'at-least at-least)
                             (list expected))
                           (list
                            :actual-args-number actual)))))
  (cl-loop for e in expected-args
           for a in actual-args
           for i below (length expected-args)
           until (eq e '**)
           do (mock-verify-arg e a i funcsym expected-args actual-args))
  (let ((actual-times (or (get funcsym 'mock-call-count) 0)))
    (and expected-times (/= expected-times actual-times)
         (signal 'mock-error (list (cons funcsym expected-args)
                                   :expected-times expected-times
                                   :actual-times actual-times)))))
;;;; stub/mock provider
(defun mock-protect (body-fn)
  "The substance of `with-mock' macro.
Prepare for mock/stub, call BODY-FN, and teardown mock/stub.

For developer:
When you adapt Emacs Lisp Mock to a testing framework,
wrap test method around this function."
  (let (mock-verify-list
        mock--stubbed-functions
        mock--mocked-functions
        (mock--in-mocking t)
        (any-error t))
    ;; (setplist 'mock-original-func nil)
    ;; (setplist 'mock-call-count nil)
    (unwind-protect
        (prog1
            (funcall body-fn)
          (setq any-error nil))
      ;; FIXME: `delete-dups' is for backward compatibility with `.elc'
      ;; compiled with an old version of `el-mock' since those did
      ;; (push ',function mock--stubbed-functions) after `stub/setup'.
      (mapc #'stub/teardown (delete-dups mock--stubbed-functions))
      (unless any-error
        (mock-verify)))))

;;;; message hack
(defun mock-suppress-redefinition-message (func)
  "Erase \"ad-handle-definition: `FUNC' got redefined\" message."
  (funcall func))
(define-error 'mock-syntax-error "Mock syntax error")

;;;; User interface
(defmacro with-mock (&rest body)
  "Execute the forms in BODY containing `mock' and `stub' forms.
The value returned is the value of the last form in BODY.
After executing BODY, mocks and stubs are guaranteed to be released.

Example:
  (with-mock
    (stub fooz => 2)
    (fooz 9999))                  ; => 2"
  (declare (indent 0))
  `(mock-protect
    (lambda () ,@body)))

(define-obsolete-function-alias 'with-stub #'with-mock "1.26?")

(defmacro stub (function &rest rest)    ;FIXME: `mock-' namespace?
  ;; checkdoc-params: (rest)
  "Create a stub for FUNCTION.
Stubs are temporary functions which accept any arguments
and return constant value.
Stubs are removed outside `with-mock' (`with-stub' is an alias) and `mocklet'.

Synopsis:
* (stub FUNCTION)
  Create a FUNCTION stub which returns nil.
* (stub FUNCTION => RETURN-VALUE)
  Create a FUNCTION stub which returns RETURN-VALUE.

RETURN-VALUE is evaluated when executing the mocked function.

Example:
  (with-mock
    (stub foo)
    (stub bar => 1)
    (and (null (foo)) (= (bar 7) 1)))     ; => t"
  (let ((value (cond ((plist-get rest '=>))
                     ((memq '=> rest) nil)
                     ((null rest) nil)
                     (t (signal 'mock-syntax-error '("Use `(stub FUNC)' or `(stub FUNC => RETURN-VALUE)'"))))))
    `(if (not mock--in-mocking)
         (error "Do not use `stub' outside")
       (mock--stub-setup ',function (lambda (&rest _) ,value)))))

(defmacro mock (func-spec &rest rest)
    ;; checkdoc-params: (rest)
  "Create a mock for function described by FUNC-SPEC.
Mocks are temporary functions which accept specified arguments
and return constant value.
If mocked functions are not called or called by different arguments,
an `mock-error' occurs.
Mocks are removed outside `with-mock' and `mocklet'.

Synopsis:
* (mock (FUNCTION ARGS...))
  Create a FUNCTION mock which returns nil.
* (mock (FUNCTION ARGS...) => RETURN-VALUE)
  Create a FUNCTION mock which returns RETURN-VALUE.
* (mock (FUNCTION ARGS...) :times N)
  FUNCTION must be called N times.
* (mock (FUNCTION ARGS...) => RETURN-VALUE :times N)
  Create a FUNCTION mock which returns RETURN-VALUE.
  FUNCTION must be called N times.

ARGS that are of a form `*' and `**' are wildcards.  The wildcard `*'
accepts any value for that argument position.  The wildcard `**' accepts
any number of arguments (including 0), effectively suppressing any further ARGS.

ARGS that are of a from (~= MATCHER [EXPLAINER]) are matchers.  When a
matcher is specified as an expected argument it is used to verify the
actual value of the argument.  This is as opposed to comparing the
actual and expected values of an argument with `equal'.  MATCHER is a
function that is called with a single argument (the actual value of the
argument).  A match is considered to be successful when MATCHER returns
non-nil.  Otherwise `mock-error' occurs.  When the optional EXPLAINER is
specified it is called with the same argument.  EXPLAINER and should
return a string explaining why the match of the argument has failed.
When EXPLAINER has not been specified and MATCHER is a symbol with a
property `mock-explainer' (or `ert-explainer') the value of the property
is used to explain the match failure.  Both MATCHER and EXPLAINER have
their closures created when the mock is created to preserve lexical
context.

ARGS that are not `*' are evaluated when the mock is verified,
i.e. upon leaving the enclosing `with-mock' form.  ARGS are
evaluated using dynamic scoping.  The RETURN-VALUE is evaluated
when executing the mocked function.

Example:
  (with-mock
    (mock (f * 2) => 3)
    (mock (g 3))
    (and (= (f 9 2) 3) (null (g 3))))     ; => t
  (with-mock
    (mock (g 0 3))
    (g 0 7))                              ; (mock-error (g 0 3)
                                          ;             (g 0 7)
                                          ;             :arg-index 1
                                          ;             :expected-arg 3
                                          ;             :actual-arg 7)
  (let ((x 13))
    (with-mock
      (mock (h (~= (lambda (arg) (<= x arg (+ x 2)))) (~= #\='stringp) => \='pass)
      (h 14 \"test\")))                   ; => \='pass
  (let ((x 13))
    (with-mock
      (mock (h (~= (lambda (arg) (<= x arg (+ x 2)))
                   (lambda (arg)
                     (format \"Expected arg to be between %s and %s but got %s\"
                             x (+ x 2) arg)))) => \='pass)
      (h 22)))
   ; (mock-error (h (~= #f(lambda (arg) [(x 13)] (<= x arg (+ x 2)))))
   ;             (h 22)
   ;             :arg-index 0
   ;             :failing-matcher #f(lambda (arg) [(x 13)] (<= x arg (+ x 2)))
   ;             :failing-arg 22
   ;             :explanation \"Expected arg to be between 13 and 15 but got 22\")"
  (let* ((times (plist-get rest :times))
         (value (cond ((plist-get rest '=>))
                      ((memq '=> rest) nil)
                      ((null rest) nil)
                      ((not times) (signal 'mock-syntax-error '("Use `(mock FUNC-SPEC)' or `(mock FUNC-SPEC => RETURN-VALUE)'")))))
         (matchers (apply #'append
                         (delq nil
                               (mapcar (lambda (arg)
                                         (when (eq (car-safe arg) '~=)
                                           (cdr arg)))
                                       (cdr func-spec)))))
         (matchers-var (make-symbol "matchers-var")))
    `(if (not mock--in-mocking)
         (error "Do not use `mock' outside")
       (let ((,matchers-var (list ,@matchers)))
         (mock/setup (cons ',(car func-spec)
                           (mapcar (lambda (arg)
                                     (if (eq (car-safe arg) '~=)
                                         (cons (car arg)
                                               (mapcar (lambda (_)
                                                         (pop ,matchers-var))
                                                       (cdr arg)))
                                       arg))
                                   ',(cdr func-spec)))
                     ',value ,times)))))

(defmacro not-called (function)    ;FIXME: `mock-' namespace?
  "Create a not-called mock for FUNCTION.
Not-called mocks are temporary functions which raises an error when called.
If not-called functions are called, an `mock-error' occurs.
Not-called mocks are removed outside `with-mock' and `mocklet'.

Synopsis:
* (not-called FUNCTION)
  Create a FUNCTION not-called mock.

Example:
  (with-mock
    (not-called f)
    t)     ; => t
  (with-mock
    (not-called g)
    (g 7)) ; => (mock-error called)"
  (let ()
    `(if (not mock--in-mocking)
         (error "Do not use `not-called' outside")
       (mock--stub-setup ',function
                         (lambda (&rest _) (signal 'mock-error '(called)))))))


(defun mock-parse-spec (spec)
  "Parse a `mocklet' SPEC to series of `mock's and `stub's."
  (cons 'progn
        (mapcar (lambda (args)
                  (if (eq (cadr args) 'not-called)
                      `(not-called ,(car args))
                    (cons (if (consp (car args)) 'mock 'stub)
                          args)))
                spec)))

(defun mocklet-function (spec body-func)
  ;; FIXME: This is kept for compatibility with .elc files compiled with old
  ;; versions of `el-mock'.  Is it worth keeping?
  (declare (obsolete with-mock "el-mock-1.26?"))
  (with-mock
    (eval (mock-parse-spec spec) t)
    (funcall body-func)))

(defmacro mocklet (speclist &rest body)
  "`let'-like interface of `with-mock', `mock', `not-called' and `stub'.

Create mocks and stubs described by SPECLIST then execute the forms in BODY.
SPECLIST is a list of mock/not-called/stub spec.
The value returned is the value of the last form in BODY.
After executing BODY, mocks and stubs are guaranteed to be released.

Synopsis of spec:
Spec is arguments of `mock', `not-called' or `stub'.
* ((FUNCTION ARGS...))                  : mock which returns nil
* ((FUNCTION ARGS...) => RETURN-VALUE)  ; mock which returns RETURN-VALUE
* ((FUNCTION ARGS...) :times N )        ; mock to be called N times
* ((FUNCTION ARGS...) => RETURN-VALUE :times N )  ; mock to be called N times
* (FUNCTION)                            : stub which returns nil
* (FUNCTION => RETURN-VALUE)            ; stub which returns RETURN-VALUE
* (FUNCTION not-called)                 ; not-called FUNCTION

ARGS that are of a form `*' and `**' are wildcards.  The wildcard `*'
accepts any value for that argument position.  The wildcard `**' accepts
any number of arguments (including 0), effectively suppressing any further ARGS.

ARGS that are of a from (~= MATCHER [EXPLAINER]) are matchers.  When a
matcher is specified as an expected argument it is used to verify the
actual value of the argument.  This is as opposed to comparing the
actual and expected values of an argument with `equal'.  MATCHER is a
function that is called with a single argument (the actual value of the
argument).  A match is considered to be successful when MATCHER returns
non-nil.  Otherwise `mock-error' occurs.  When the optional EXPLAINER is
specified it is called with the same argument.  EXPLAINER and should
return a string explaining why the match of the argument has failed.
When EXPLAINER has not been specified and MATCHER is a symbol with a
property `mock-explainer' (or `ert-explainer') the value of the property
is used to explain the match failure.  Both MATCHER and EXPLAINER have
their closures created when the mock is created to preserve lexical
context.

ARGS that are neither `*' nor `**' are evaluated when the mock is
verified, i.e. upon leaving the enclosing `with-mock' form.  ARGS are
evaluated using dynamic scoping.  The RETURN-VALUE is evaluated when
executing the mocked function.

Example:
  (let ((x 13))
    (mocklet (((mock-nil 1))
              ((mock-1 *) => 1)
              ((mock-2 **) => 1 :times 2)
              ((mock-3 (~= (lambda (arg) (<= arg x))) => 1))
              (stub-nil)
              (stub-2 => 2))
      (and (null (mock-nil 1))    (= (mock-1 4) 1)
           (= (mock-2 1 2 3) 1)   (= (mock-2 \='any) 1)
           (= (mock-3 4) 1)
           (null (stub-nil \\='any)) (= (stub-2) 2))) ; => t"
  (declare (indent 1))
  `(with-mock
     ,(mock-parse-spec speclist)
     ,@body))

(define-obsolete-function-alias 'stublet #'mocklet "1.26?")

(provide 'el-mock)

;;; el-mock.el ends here
