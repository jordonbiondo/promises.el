;;; promises.el --- Promises -*- lexical-binding: t; -*-
;;
;; Filename: promises.el
;; Description: Promises for Emacs
;; Author: Jordon Biondo <jordonbiondo@gmail.com>
;; Copyright (c) 2015 Jordon Biondo <jordonbiondo@gmail.com>
;; Version: 0.0.1
;; URL: https://github.com/jordonbiondo/promises.el
;; Package-Requires: ((async "20150818") (dash "2.11.0") (cl-lib "0.3") (emacs "24.3"))
;; Keywords: convenience
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Promises for Emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The MIT License (MIT)
;;
;; Copyright (c) 2015 Jordon Biondo
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-and-compile
  (require 'dash)
  (require 'async))

(defun promise--resolve(prom val)
  (puthash :resolve val prom)
  (puthash :done t prom)
  (puthash :resolved t prom)
  (promise--notify prom))

(defun promise--reject(prom val)
  (puthash :reject val prom)
  (puthash :done t prom)
  (puthash :rejected t prom)
  (promise--notify prom))

(defun promise--notify (prom)
  (let ((listeners (gethash :listeners prom)))
    (dolist (listener listeners)
      (promise--kickoff listener))))

(defun promise--kickoff (prom)
  (let ((delay (gethash :delay prom)))
    (if delay
        (puthash :timer
                 (run-with-timer
                  delay nil
                  (lambda ()
                    (apply (gethash :perform prom) nil)))
                 prom)
      (apply (gethash :perform prom) nil))))

(defun make-promise (func)
  (let ((obj (make-hash-table :test 'equal :size 10)))
    (let ((resolve (lambda (val) (promise--resolve obj val)))
          (reject (lambda (val) (promise--reject obj val))))
      (puthash :promisep t obj)
      (puthash :perform
               (lambda ()
                 (condition-case err
                     (apply func (list resolve reject))
                   (error (funcall reject err))))
               obj)
      obj)))

(defun promise (func)
  "Create a new promise that will execute FUNC.

FUNC must be in the form (lambda (resolve reject) ...)."
  (let ((obj (make-promise func)))
    (promise--kickoff obj)
    obj))

(defmacro promise* (args &rest body)
  "Convenience wrapper for `promise'.

BODY is the body of the promise's function.

ARGS should be a list of two symbols in the form of (resolve reject).

Unlike `promise', the resolve and reject symbols are temporarily bound
as functions which you can call directly instead of using funcall or apply.

Example:

    (promise* (resolve reject)
      (let ((value (get-some-value)))
        (if (> value 0)
            (resolve value)
          (reject \"Invalid Value\"))))

this is equivalent to:

    (promise
     (lambda (resolve reject)
       (let ((value (get-some-value)))
         (if (> value 0)
             (funcall resolve value)
           (funcall reject \"Invalid Value\")))))"
  (declare (indent 1))
  (let ((resolve-param (make-symbol (concat (symbol-name (car args)) "-param")))
        (reject-param (make-symbol (concat (symbol-name (cadr args)) "-param"))))

    `(promise
      (lambda (,resolve-param ,reject-param)
        (cl-labels ((,(car args) (value) (funcall ,resolve-param value))
                    (,(cadr args) (value) (funcall ,reject-param value)))
          ,@body)))))

(defun resolved-promise (val)
  "Create a promise immediately resolved with VAL."
  (promise* (resolve reject)
    (resolve val)))

(defun rejected-promise (val)
  "Create a promise immediately rejecting VAL."
  (promise* (resolve reject)
    (reject val)))

(defun delay (func)
  "Create a promise that will execute on a 0 second timer.

Effectively this will wait to run until the current stack clears."
  (let ((p (make-promise func)))
    (puthash :delay 0 p)
    (promise--kickoff p)
    p))

(defmacro delay* (args &rest body)
  (declare (indent 1))
  (let ((resolve-param (make-symbol (concat (symbol-name (car args)) "-param")))
        (reject-param (make-symbol (concat (symbol-name (cadr args)) "-param"))))
    `(delay
      (lambda (,resolve-param ,reject-param)
        (cl-labels ((,(car args) (value) (funcall ,resolve-param value))
                    (,(cadr args) (value) (funcall ,reject-param value)))
          ,@body)))))

(defun delay-time (seconds func)
  "Create a promise that will execute after SECONDS."
  (declare (indent 1))
  (let ((p (make-promise func)))
    (puthash :delay seconds p)
    (promise--kickoff p)
    p))

(defmacro delay-time* (seconds args &rest body)
  (declare (indent 2))
  (let ((resolve-param (make-symbol (concat (symbol-name (car args)) "-param")))
        (reject-param (make-symbol (concat (symbol-name (cadr args)) "-param"))))
    `(delay-time ,seconds
       (lambda (,resolve-param ,reject-param)
         (cl-labels ((,(car args) (value) (funcall ,resolve-param value))
                     (,(cadr args) (value) (funcall ,reject-param value)))
           ,@body)))))

(defun promisep (promise)
  (and (hash-table-p promise)
       (gethash :promisep promise)
       (gethash :perform promise)))

(defun promise--listen (listener notifier)
  (if (gethash :done notifier)
      (promise--kickoff listener)
    (push listener (gethash :listeners notifier))))


(defun then (promise func)
  "After PROMISE resolves or is rejected, run FUNC.

FUNC will be passed two parameters, the error, if any, from PROMISE,
and the resolved value from PROMISE, if any.

A new promise is returned that will resolve to the return value of FUNC,
or reject on an error that occurs in FUNC."
  (let ((obj (make-promise
              (lambda (resolve reject)
                (let ((err (gethash :reject promise))
                      (value (gethash :resolve promise)))
                  (let ((output (apply func (list err value))))
                    (if (promisep output)
                        (then output
                              (lambda (err value)
                                (if err
                                    (funcall reject err)
                                  (funcall resolve value))))
                      (funcall resolve output))))))))
    (promise--listen obj promise)
    obj))

(defun then2 (promise func &optional err-func)
  "After PROMISE resolves or is rejected, run FUNC.

FUNC will be passed two parameters, the error, if any, from PROMISE,
and the resolved value from PROMISE, if any.

A new promise is returned that will resolve to the return value of FUNC,
or reject on an error that occurs in FUNC."
  (let ((obj (make-promise
              (lambda (resolve reject)
                (let ((err (gethash :reject promise))
                      (value (gethash :resolve promise)))
                  (let ((output nil))
                    (if (gethash :rejected promise)
                        (if err-func
                            (setq output (apply err-func (list err)))
                          (setq output (promise (lambda (resolve reject) (funcall reject err)))))
                      (if func
                          (setq output (apply func (list value)))
                        (setq output (resolved-promise value))))
                    (if (promisep output)
                        (then output
                              (lambda (err value)
                                (if err
                                    (funcall reject err)
                                  (funcall resolve value))))
                      (funcall resolve output))))))))
    (promise--listen obj promise)
    obj))

(defmacro then* (promise args &rest body)
  "Convenience wrapper for `then'.

ARGS and BODY are used for the promises function.
Example:

    (then* promise (err value)
      (+ value 2))

this is equivalent to:

    (then promise (lambda (err value)
            (+ value 2)))"
  (declare (indent 2))
  `(then ,promise (lambda ,args ,@body)))

(defun promisify (func &optional n)
  "Promisify a callback-based function FUNC.

FUNC should take a callback as its Nth parameter.

If N is not given, it is assumed FUNC takes a callback as its
last parameter.

The promise will resolve with the return value of FUNC and the arguments
typically passed to the callback.

Example:

    (defun foo (a b callback d)
      (funcall callback (+ a b d)))

    (then (funcall (promisify 'foo 2) 1 2 3)
          (lambda (err value)
            (print value))) ; prints '(6)
"
  (lambda (&rest args)
    (promise
     (lambda (resolve reject)
       (ignore reject)
       (let* ((output-value nil)
              (callback (lambda (&rest cbargs) (funcall resolve (append (list output-value) cbargs)))))
         (let ((args (if (and n (< n (length args)))
                         (if (zerop n)
                             (append (list callback) args)
                           (push callback (cdr (nthcdr (1- n) args)))
                           args)
                       (append args (list callback)))))
           (setq output-value (apply func args))))))))

(defun promise-async (func)
  "Create an asynchronous promise using `async-start'

FUNC will be evaluated in another emacs process and
the promise will resolve with the return value, or reject
with any errors that may occur."
  (let ((wrapped-func
         (lambda ()
           (condition-case err
               (list 'val (apply func nil))
             (error (list 'err err))))))
    (promise* (ok nope)
      (async-start
       wrapped-func
       (lambda (val)
         (if (equal (car val) 'val)
             (ok (cadr val))
           (nope (cadr val))))))))

(defmacro promise-async* (&rest body)
  `(promise-async (lambda () ,@body)))

(defun promise-all (promises)
  (let ((values (make-list (length promises) nil))
        (real-promises (-filter 'promisep promises)))
    (promise* (resolve reject)
      (dotimes (n (length promises))
        (let ((prom (nth n promises)))
          (unless (promisep prom)
            (setq prom (resolved-promise prom)))
          (then prom
                (lambda (err value)
                  (if err
                      (reject err)
                    (setf (nth n values) value)
                    (when (-all? (lambda (p) (gethash :done p)) real-promises)
                      (resolve values))))))))))

(provide 'promises)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; promises.el ends here
