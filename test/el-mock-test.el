;;; el-mock-test.el --- Tests for el-mock.el  -*- lexical-binding: t; -*-


;;; Code:

(require 'el-mock)
(require 'ert-expectations)

(declare-function foo "el-mock-test")
(declare-function foox "el-mock-test")
(declare-function hogehoges "el-mock-test")
(declare-function fooz "el-mock-test")
(declare-function hoge "el-mock-test")
(declare-function me "el-mock-test")
(declare-function foom "el-mock-test")
(declare-function bar "el-mock-test")
(declare-function f "el-mock-test")
(declare-function vi "el-mock-test")
(declare-function test1 "el-mock-test")
(declare-function defined-func "el-mock-test")
(declare-function a "el-mock-test")
(declare-function b "el-mock-test")
(declare-function blah "el-mock-test")
(declare-function fugaga "el-mock-test")

(defvar el-mock-test-var)
(defvar mock-error)

(defun el-mock-test--wrap-retval (orig-fun &rest args)
  "Wrap value of a call of ORIG-FUN with ARGS."
  (concat "[" (apply orig-fun args) "]"))

(defun el-mock-test--never-match-1 (_)
  "Return nil, but the symbol has `mock-explainer' propoerty."
  nil)
(function-put #'el-mock-test--never-match-1
              'mock-explainer
              (lambda (arg)
                (format "never-match-1 %S" arg)))

(defun el-mock-test--never-match-2 (_)
  "Return nil, but the symbol has `ert-explainer' property."
  nil)
(function-put #'el-mock-test--never-match-2
              'ert-explainer
              (lambda (arg)
                (format "never-match-2 %S" arg)))

(expectations
 (desc "stub setup/teardown")
 (expect 2
   (mock--stub-setup 'foo (lambda (&rest _) 2))
   (prog1
       (foo 1 2 3)
     (stub/teardown 'foo)))
 (expect nil
         (mock--stub-setup 'foox (lambda (&rest _) 2))
         (foox 1 2 3)
         (stub/teardown 'foox)
         (fboundp 'foox))
 (desc "with-mock interface")
 (expect 9801
         (with-mock
           9801))
 (desc "stub macro")
 (expect nil
         (with-mock
           (stub hogehoges)
           (hogehoges 75)))
 (expect 2
         (with-mock
           (stub fooz => 2)
           (fooz 9999)))
 (expect nil
         (with-mock
           (stub fooz => 2)
           (fooz 3))
         (fboundp 'fooz))
 (expect nil
         (with-mock
           (stub hoge)                  ;omission of return value
           (hoge)))
 (expect 'hoge
         (with-mock
           (stub me => 'hoge)
           (me 1)))
 (expect 34
         (with-mock
           (stub me => (+ 3 31))
           (me 1)))
 ;; ;; TODO defie mock-syntax-error / detect mock-syntax-error in expectations
 ;; (desc "abused stub macro")
 ;; (expect (error mock-syntax-error '("Use `(stub FUNC)' or `(stub FUNC => RETURN-VALUE)'"))
 ;;         (with-mock
 ;;          (stub fooz 7)))
 (expect (error-message "Do not use `stub' outside")
         (let (mock--in-mocking  ; while executing `expect', `in-mocking' is t.
               (text-quoting-style 'grave))
           (stub hahahaha)))
 (desc "mock macro")
 (expect 2
         (with-mock
           (mock (foom 5) => 2)
           (foom 5)))
 (expect 3
         (with-mock
           (mock (foo 5) => 2)
           (mock (bar 7) => 1)
           (+ (foo 5) (bar 7))))
 (expect 3
         (cl-flet ((plus () (+ (foo 5) (bar 7))))
           (with-mock
             (mock (foo 5) => 2)
             (mock (bar 7) => 1)
             (plus))))
 (expect 1
         (with-mock
           (mock (f * 2) => 1)
           (f 1 2)))
 (expect 1
         (with-mock
           (mock (f * (1+ 1)) => (+ 0 1)) ;evaluated
           (f 1 2)))
 (expect nil
         (with-mock
           (mock (f 2))                 ;omission of return value
           (f 2)))
 (expect 'hoge
         (with-mock
           (mock (me 1) => 'hoge)
           (me 1)))
 (expect 34
         (with-mock
           (mock (me 1) => (+ 3 31))
           (me 1)))
 (expect nil
         (let ((el-mock-test-var 11))
           (with-mock
             (setq el-mock-test-var 22)
             (mock (me el-mock-test-var)) ; not yet evaluated
             (setq el-mock-test-var 33)
             (me 33))))

 (desc "unfulfilled mock")
 (expect (error mock-error '((foom 5) (foom 6)
                             :arg-index 0 :expected-arg 5 :actual-arg 6))
         (with-mock
           (mock (foom 5) => 2)
           (foom 6)))
 (expect (error mock-error '((bar 7) (bar 8)
                             :arg-index 0 :expected-arg 7 :actual-arg 8))
         (with-mock
           (mock (foo 5) => 2)
           (mock (bar 7) => 1)
           (+ (foo 5) (bar 8))))
 (expect (error mock-error '(not-called foo))
         (with-mock
           (mock (foo 5) => 2)))
 (expect (error mock-error '(not-called foo))
         (with-mock
           (mock (vi 5) => 2)
           (mock (foo 5) => 2)
           (vi 5)))
 (expect (error mock-error '((f 2) (f 4)
                             :arg-index 0 :expected-arg 2 :actual-arg 4))
         (with-mock
           (mock (f 2))                 ;omission of return value
           (f 4)))
 (expect (error-message "Error-in-test1")
         (defun test1 () (error "Error-in-test1"))
         (with-mock
           (mock (test2))
           (test1)))
 ;; (desc "abused mock macro")
 ;; (expect (error mock-syntax-error '("Use `(mock FUNC-SPEC)' or `(mock FUNC-SPEC => RETURN-VALUE)'"))
 ;;         (with-mock
 ;;          (mock (fooz) 7)))
 (expect (error-message "Do not use `mock' outside")
         (let (mock--in-mocking  ; while executing `expect', `in-mocking' is t.
               (text-quoting-style 'grave))
           (mock (hahahaha))))

 (desc "mock with stub")
 (expect 8
         (with-mock
           (mock (f 1 2) => 3)
           (stub hoge => 5)
           (+ (f 1 2) (hoge 'a))))
 (expect (error mock-error '((f 1 2) (f 3 4)
                             :arg-index 0 :expected-arg 1 :actual-arg 3))
         (with-mock
           (mock (f 1 2) => 3)
           (stub hoge => 5)
           (+ (f 3 4) (hoge 'a))))

 (desc "with-stub is an alias of with-mock")
 (expect 'with-mock
         (symbol-function 'with-stub))

 (desc "stublet is an alias of mocklet")
 (expect 'mocklet
         (symbol-function 'stublet))

 (desc "mock-parse-spec")
 (expect '(progn
            (mock (f 1 2) => 3)
            (stub hoge => 5))
         (mock-parse-spec
          '(((f 1 2) => 3)
            (hoge    => 5))))
 (expect '(progn
            (not-called g))
         (mock-parse-spec
          '((g not-called))))

 (desc "mocklet")
 (expect 8
         (mocklet (((f 1 2) => 3)
                   (hoge    => 5))
           (+ (f 1 2) (hoge 'a))))
 (expect 2
         (mocklet ((foo => 2))
           (foo 1 2 3)))
 (expect 3
         (defun defined-func (_x) 3)
         (prog1
             (mocklet ((defined-func => 3))
               (defined-func 3))
           (fmakunbound 'defined-func)))
 (expect nil
         (mocklet ((f))                 ;omission of return value
           (f 91)))
 (expect nil
         (mocklet (((f 76)))            ;omission of return value
           (f 76)))
 (expect 5
         (mocklet ((a => 3)
                   (b => 2))
           1                            ;multiple exprs
           (+ (a 999) (b 7))))

 (desc "stub for defined function")
 (expect "xxx"
         (defun blah (x) (* x 2))
         (prog1
             (mocklet ((blah => "xxx"))
               (blah "xx"))
           (fmakunbound 'blah)))
 (expect t
         (defun blah (x) (* x 2))
         (prog1
             (let ((orig (symbol-function 'blah)))
               (mocklet ((blah => "xx"))
                 (blah "xx"))
               (equal orig (symbol-function 'blah)))
           (fmakunbound 'blah)))

 (desc "stub for adviced function")
 (expect "xxx"
         (mock-suppress-redefinition-message ;silence redefinition warning
          (lambda ()
            (defun fugaga (x) (* x 2))
            (advice-add 'fugaga :around #'el-mock-test--wrap-retval)
            (prog1
                (mocklet ((fugaga => "xxx"))
                  (fugaga "aaaaa"))
              (fmakunbound 'fugaga)))))
 (expect t
         (mock-suppress-redefinition-message
          (lambda ()
            (defun fugaga (x) (* x 2))
            (advice-add 'fugaga :around #'el-mock-test--wrap-retval)
            (prog1
                (let ((orig (symbol-function 'fugaga)))
                  (mocklet ((fugaga => "xx"))
                    (fugaga "aaaaa"))
                  (equal orig (symbol-function 'fugaga)))
              (fmakunbound 'fugaga)))))

 (desc "mock for adviced function")
 (expect "xx"
         (mock-suppress-redefinition-message
          (lambda ()
            (defun fugaga (x) (* x 2))
            (advice-add 'fugaga :around #'el-mock-test--wrap-retval)
            (prog1
                (mocklet (((fugaga "aaaaa") => "xx"))
                  (fugaga "aaaaa"))
              (fmakunbound 'fugaga)))))
 (expect t
         (mock-suppress-redefinition-message
          (lambda ()
            (defun fugaga (x) (* x 2))
            (advice-add 'fugaga :around #'el-mock-test--wrap-retval)
            (prog1
                (let ((orig (symbol-function 'fugaga)))
                  (mocklet (((fugaga "aaaaa") => "xx"))
                    (fugaga "aaaaa"))
                  (equal orig (symbol-function 'fugaga)))
              (fmakunbound 'fugaga)))))
 (desc "not-called macro")
 (expect 'ok
         (with-mock
           (not-called foom)
           'ok))
 (desc "mocklet/notcalled")
 (expect 'ok
         (mocklet ((foom not-called))
           'ok))
 (desc "unfulfilled not-called")
 (expect (error mock-error '(called))
         (with-mock
           (not-called hoge)
           (hoge 1)))
 (desc "abused not-called macro")
 (expect (error-message "Do not use `not-called' outside")
         (let (mock--in-mocking  ; while executing `expect', `in-mocking' is t.
               (text-quoting-style 'grave))
           (not-called hahahaha)))
 (desc "not-called for adviced function")
 (expect "not-called"
         (mock-suppress-redefinition-message ;silence redefinition warning
          (lambda ()
            (defun fugaga (x) (* x 2))
            (advice-add 'fugaga :around #'el-mock-test--wrap-retval)
            (prog1
                (mocklet ((fugaga not-called))
                  "not-called")
              (fmakunbound 'fugaga)))))
 (expect t
         (mock-suppress-redefinition-message
          (lambda ()
            (defun fugaga (x) (* x 2))
            (advice-add 'fugaga :around #'el-mock-test--wrap-retval)
            (prog1
                (let ((orig (symbol-function 'fugaga)))
                  (mocklet ((fugaga not-called))
                    "not-called")
                  (equal orig (symbol-function 'fugaga)))
              (fmakunbound 'fugaga)))))
 (desc ":times mock")
 (expect 'ok
         (with-mock
           (mock (foo 1) => 2 :times 2)
           (foo 1)
           (foo 1)
           'ok))
 (expect 'ok
         (with-mock
           (mock (foo *) => 2 :times 2)
           (foo 1)
           (foo 2)
           'ok))
 (expect 'ok
         (with-mock
           (mock (foo 1) :times 2)
           (foo 1)
           (foo 1)
           'ok))
 (expect 'ok
         (with-mock
           (mock (foo *) :times 2)
           (foo 1)
           (foo 2)
           'ok))
 ;; FIXME
 ;; (expect 'ok
 ;;   (with-mock
 ;;     (mock (foo 1) => 2 :times 2)
 ;;     (foo 1)
 ;;     (foo 1)
 ;;     (foo 2)
 ;;     'ok))
 (expect (error mock-error '((foo 1) :expected-times 2 :actual-times 1))
         (with-mock
           (mock (foo 1) :times 2)
           (foo 1)
           'ok))
 (expect (error mock-error '((foo *) :expected-times 2 :actual-times 1))
         (with-mock
           (mock (foo *) :times 2)
           (foo 1)
           'ok))
 (expect (error mock-error '((foo 1) (foo 2)
                             :arg-index 0 :expected-arg 1 :actual-arg 2))
         (with-mock
           (mock (foo 1) :times 2)
           (foo 2)
           'ok))
 (expect (error mock-error '(not-called foo))
         (with-mock
           (mock (foo 1) :times 2)
           'ok))
 (expect (error mock-error '((foo 1) :expected-times 2 :actual-times 1))
         (with-mock
           (mock (foo 1) => 2 :times 2)
           (foo 1)
           'ok))
 (expect (error mock-error '((foo *) :expected-times 2 :actual-times 1))
         (with-mock
           (mock (foo *) => 2 :times 2)
           (foo 1)
           'ok))
 (expect (error mock-error '((foo 1) (foo 2)
                             :arg-index 0 :expected-arg 1 :actual-arg 2))
         (with-mock
           (mock (foo 1) => 2 :times 2)
           (foo 2)
           'ok))
 (expect (error mock-error '(not-called foo))
         (with-mock
           (mock (foo 1) => 2 :times 2)
           'ok))

 (desc "too few arguments")
 (expect (error mock-error '((foo 1) (foo)
                             :expected-args-number 1 :actual-args-number 0))
         (with-mock
           (mock (foo 1))
           (foo)))

 (desc "wildcards")
 (expect 'ok
         (with-mock
           (mock (foo *) => 'ok :times 2)
           (foo 1)
           (foo 'any)))
 (expect 'ok
         (with-mock
           (mock (foo 1 * 3) => 'ok :times 2)
           (foo 1 2 3)
           (foo 1 'any 3)))
 (expect (error mock-error '((foo *) (foo 1 2)
                             :expected-args-number 1 :actual-args-number 2))
         (with-mock
           (mock (foo *) => 'ok)
           (foo 1 2)))
 (expect (error mock-error '((foo *) (foo)
                             :expected-args-number 1 :actual-args-number 0))
         (with-mock
           (mock (foo *) => 'ok)
           (foo)))
 (expect (error mock-error '((foo 1 * 3) (foo 2 2 2)
                             :arg-index 0 :expected-arg 1 :actual-arg 2))
         (with-mock
           (mock (foo 1 * 3) => 'ok)
           (foo 2 2 2)))
  (expect (error mock-error '((foo 1 * 3) (foo 1 2 2)
                             :arg-index 2 :expected-arg 3 :actual-arg 2))
         (with-mock
           (mock (foo 1 * 3) => 'ok)
           (foo 1 2 2)))
 (expect 'ok
         (with-mock
           (mock (foo **) => 'ok :times 3)
           (foo)
           (foo 'any)
           (foo 'any 'any)))
 (expect 'ok
         (with-mock
           (mock (foo 1 **) => 'ok :times 3)
           (foo 1)
           (foo 1 2)
           (foo 1 'any 3)))
 (expect (error mock-error '((foo 1 **) (foo 2 2)
                             :arg-index 0 :expected-arg 1 :actual-arg 2))
         (with-mock
           (mock (foo 1 **) => 'ok)
           (foo 2 2)))
 (expect (error mock-error '((foo 1 **) (foo)
                             :expected-args-number at-least 1 :actual-args-number 0))
         (with-mock
           (mock (foo 1 **) => 'ok)
           (foo)))
 (expect 'ok
         (mocklet (((foo *) => 'ok :times 2))
           (foo 1)
           (foo 'any)))
 (expect 'ok
         (mocklet (((foo 1 * 3) => 'ok :times 2))
           (foo 1 2 3)
           (foo 1 'any 3)))
 (expect (error mock-error '((foo *) (foo 1 2)
                             :expected-args-number 1 :actual-args-number 2))
         (mocklet (((foo *) => 'ok))
           (foo 1 2)))
 (expect (error mock-error '((foo *) (foo)
                             :expected-args-number 1 :actual-args-number 0))
         (mocklet (((foo *) => 'ok))
           (foo)))
 (expect (error mock-error '((foo 1 * 3) (foo 2 2 2)
                             :arg-index 0 :expected-arg 1 :actual-arg 2))
         (mocklet (((foo 1 * 3) => 'ok))
           (foo 2 2 2)))
 (expect 'ok
         (mocklet (((foo **) => 'ok :times 3))
           (foo)
           (foo 'any)
           (foo 'any 'any)))
 (expect 'ok
         (mocklet (((foo 1 **) => 'ok :times 3))
           (foo 1)
           (foo 1 2)
           (foo 1 'any 3)))
 (expect (error mock-error '((foo 1 **) (foo 2 2)
                             :arg-index 0 :expected-arg 1 :actual-arg 2))
         (mocklet (((foo 1 **) => 'ok))
           (foo 2 2)))
 (expect (error mock-error '((foo 1 **) (foo)
                             :expected-args-number at-least 1 :actual-args-number 0))
         (mocklet (((foo 1 **) => 'ok))
           (foo)))

 (desc "matchers")
 (expect 'ok
         (with-mock
           (mock (foo (~= #'stringp)) => 'ok)
           (foo "is it ok?")))

 (expect 'ok
         (with-mock
           (mock (foo (~= #'stringp #'null)
                      (~= #'numberp #'null)
                      (~= #'consp #'null))
                 => 'ok)
           (foo "a string" 1 (cons 'bar 'baz))))

 (expect 'ok
         (with-mock
           (mock (foo (~= (lambda (arg) (<= 1 arg 3)))) => 'ok)
           (foo 2)))

 (expect 'ok
         (let ((x 3))
           (with-mock
             (mock (foo (~= (lambda (arg) (< x arg (+ x 2))))) => 'ok)
             (foo 4))))

 (expect 'ok
         (with-mock
           (mock (foo (~= (let ((x 5))
                            (lambda (arg) (< x arg (+ x 2))))))
                 => 'ok)
           (foo 6)))

 (expect 'ok
         (with-mock
           (let ((x 7))
             (mock (foo (~= (lambda (arg) (< x arg (+ x 2))))) => 'ok)
             (foo 8))))

 (expect 'ok
         (with-mock
           (let ((x 9))
             (mock (foo (~= (lambda (arg) (< x arg (+ x 2))))) => 'ok))
           (foo 10)))

 (expect 'ok
         (let ((x 11))
           (with-mock
             (mock (foo (~= (let ((y (+ x 2)))
                              (lambda (arg) (< x arg y)))))
                   => 'ok)
             (foo 12))))

 (expect (error mock-error '((foo (~= stringp))
                             (foo 13)
                             :arg-index 0
                             :failing-matcher stringp
                             :failing-arg 13))
         (with-mock
           (mock (foo (~= #'stringp)) => 'ok)
           (foo 13)))

 (expect (error mock-error '((foo 14 (~= stringp))
                             (foo 14 14)
                             :arg-index 1
                             :failing-matcher stringp
                             :failing-arg 14
                             :explanation "Expected a string but got 14"))
         (with-mock
           (mock (foo 14 (~= #'stringp (lambda (arg)
                                         (format "Expected a string but got %S" arg))))
                 => 'ok)
           (foo 14 14)))

 (expect (error mock-error '((foo (~= el-mock-test--never-match-1))
                             (foo 15)
                             :arg-index 0
                             :failing-matcher el-mock-test--never-match-1
                             :failing-arg 15
                             :explanation "never-match-1 15"))
         (with-mock
           (mock (foo (~= #'el-mock-test--never-match-1))
                 => 'ok)
           (foo 15)))

 (expect (error mock-error '((foo 16 (~= el-mock-test--never-match-2))
                             (foo 16 16)
                             :arg-index 1
                             :failing-matcher el-mock-test--never-match-2
                             :failing-arg 16
                             :explanation "never-match-2 16"))
         (with-mock
           (mock (foo 16 (~= #'el-mock-test--never-match-2))
                 => 'ok)
           (foo 16 16)))

 (expect 'ok
         (mocklet (((foo (~= #'stringp)) => 'ok))
           (foo "is it still ok?")))

 (expect 'ok
         (mocklet (((foo (~= #'stringp #'null)
                         (~= #'numberp #'null)
                         (~= #'consp #'null))
                    => 'ok))
           (foo "another string" 21 (cons 'bar 'baz))))

 (expect 'ok
         (mocklet (((foo (~= (lambda (arg) (<= 21 arg 23)))) => 'ok))
           (foo 22)))

 (expect 'ok
         (let ((x 23))
           (mocklet (((foo (~= (lambda (arg) (< x arg (+ x 2))))) => 'ok))
             (foo 24))))

 (expect 'ok
         (mocklet (((foo (~= (let ((x 25))
                               (lambda (arg) (< x arg (+ x 2))))))
                    => 'ok))
           (foo 26)))

 (expect (error mock-error '((foo (~= stringp))
                             (foo 27)
                             :arg-index 0
                             :failing-matcher stringp
                             :failing-arg 27))
         (mocklet (((foo (~= #'stringp)) => 'ok))
           (foo 27)))

 (expect (error mock-error '((foo 28 (~= stringp))
                             (foo 28 28)
                             :arg-index 1
                             :failing-matcher stringp
                             :failing-arg 28
                             :explanation "Expected a string but got 28"))
         (mocklet (((foo 28 (~= #'stringp (lambda (arg)
                                            (format "Expected a string but got %S" arg))))
                    => 'ok))
           (foo 28 28)))

 (expect (error mock-error '((foo (~= el-mock-test--never-match-1))
                             (foo 29)
                             :arg-index 0
                             :failing-matcher el-mock-test--never-match-1
                             :failing-arg 29
                             :explanation "never-match-1 29"))
         (mocklet (((foo (~= #'el-mock-test--never-match-1))
                    => 'ok))
           (foo 29)))

 (expect (error mock-error '((foo 30 (~= el-mock-test--never-match-2))
                             (foo 30 30)
                             :arg-index 1
                             :failing-matcher el-mock-test--never-match-2
                             :failing-arg 30
                             :explanation "never-match-2 30"))
         (mocklet (((foo 30 (~= #'el-mock-test--never-match-2))
                    => 'ok))
           (foo 30 30))))

(defun el-mock-test--signal ()
  "Signal error with \"Foo\"."
  (error "Foo"))


(ert-deftest preserve-stacktrace ()
  "Ensure backtrace recorded by ‘ert-run-test’ is not messed up by mocking."
  (let ((result (ert-run-test
                 (make-ert-test
                  :body (lambda ()
                          (with-mock (el-mock-test--signal)))))))
    (should (ert-test-failed-p result))
    (should (equal (ert-test-failed-condition result)
                   '(error "Foo")))
    (cond
     ((version< emacs-version "26")
      (should (equal
               (nth 0 (ert-test-failed-backtrace result))
               '(t el-mock-test--signal))))
     ((version< emacs-version "27")
      (should (equal
               (nth 2 (ert-test-failed-backtrace result))
               '(t el-mock-test--signal nil nil))))
     ((or (not (fboundp 'native-comp-available-p)) ;; Emacs 27 doesn't have native compilation
          (not (native-comp-available-p)))
      (should (equal
               (nth 2 (ert-test-failed-backtrace result))
               (record 'backtrace-frame t 'el-mock-test--signal
                       nil nil nil nil nil))))
     (t
      ;; Since Emacs 30 with native compilation the test function is wrapped
      ;; in an `interpreted-function'.
      (if (fboundp 'interpreted-function-p)
          (let* ((frame (nth 2 (ert-test-failed-backtrace result)))
                 (fun (backtrace-frame-fun frame)))
            (should (interpreted-function-p fun))
            (should (equal (aref fun 1) '((el-mock-test--signal)))))
        ;; Up to Emacs 29 with native compilation the test function is wrapped
        ;; in a `closure'.
        (should (equal
                 (nth 2 (ert-test-failed-backtrace result))
                 (record 'backtrace-frame t
                         (list 'closure
                               (list 'el-mock-test-var t)
                               nil
                               (list 'el-mock-test--signal))
                         nil nil nil nil nil))))))))

(provide 'el-mock-test)

;;; el-mock-test.el ends here
