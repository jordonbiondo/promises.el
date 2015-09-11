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

(cl-defstruct promise-obj
  resolve
  reject
  resolved
  rejected
  done
  delay
  timer
  perform
  listeners
  canceled
  kicked-off)

(defun promise--resolve (prom val)
  (setf (promise-obj-resolve prom) val)
  (setf (promise-obj-done prom) t)
  (setf (promise-obj-resolved prom) t)
  (promise--notify prom))

(defun promise--reject (prom val)
  (setf (promise-obj-reject prom) val)
  (setf (promise-obj-done prom) t)
  (setf (promise-obj-rejected prom) t)
  (promise--notify prom))

(defun promise--notify (prom)
  (let ((listeners (promise-obj-listeners prom)))
    (dolist (listener listeners)
      (promise--kickoff listener))))

(defun promise--kickoff (prom)
  (unless (or (promise-obj-canceled prom) (promise-obj-kicked-off prom))
    (let ((delay (promise-obj-delay prom)))
      (if delay
          (setf (promise-obj-timer prom)
                (run-with-timer
                 delay nil
                 (lambda ()
                   (unless (promise-obj-canceled prom)
                     (apply (promise-obj-perform prom) nil)))))
        (apply (promise-obj-perform prom) nil))
      (setf (promise-obj-kicked-off prom) t))))

(defun make-promise (func)
  (let ((obj (make-promise-obj)))
    (let ((resolve (lambda (val) (promise--resolve obj val)))
          (reject (lambda (val) (promise--reject obj val))))
      (setf (promise-obj-perform obj)
            (lambda ()
              (condition-case err
                  (apply func (list resolve reject))
                (error (funcall reject err)))))
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
  (let ((resolve-param (make-symbol (concat (symbol-name (car args)) "-arg")))
        (reject-param (make-symbol (concat (symbol-name (cadr args)) "-arg"))))

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

(defun promise-later (func)
  "Create a promise that will execute on a 0 second timer.

Effectively this will wait to run until the current stack clears."
  (let ((p (make-promise func)))
    (setf (promise-obj-delay p) 0)
    (promise--kickoff p)
    p))

(defun delay (func)
  (make-promise func))

(defmacro delay* (args &rest body)
  (declare (indent 1))
  (let ((resolve-param (make-symbol (concat (symbol-name (car args)) "-arg")))
        (reject-param (make-symbol (concat (symbol-name (cadr args)) "-arg"))))

    `(delay
      (lambda (,resolve-param ,reject-param)
        (cl-labels ((,(car args) (value) (funcall ,resolve-param value))
                    (,(cadr args) (value) (funcall ,reject-param value)))
          ,@body)))))

(defun delay-start (prom)
  (promise--kickoff prom)
  prom)

(defmacro promise-later* (args &rest body)
  (declare (indent 1))
  (let ((resolve-param (make-symbol (concat (symbol-name (car args)) "-arg")))
        (reject-param (make-symbol (concat (symbol-name (cadr args)) "-arg"))))
    `(promise-later
      (lambda (,resolve-param ,reject-param)
        (cl-labels ((,(car args) (value) (funcall ,resolve-param value))
                    (,(cadr args) (value) (funcall ,reject-param value)))
          ,@body)))))

(defun promise-later-time (seconds func)
  "Create a promise that will execute after SECONDS."
  (declare (indent 1))
  (let ((p (make-promise func)))
    (setf (promise-obj-delay p) seconds)
    (promise--kickoff p)
    p))

(defmacro promise-later-time* (seconds args &rest body)
  (declare (indent 2))
  (let ((resolve-param (make-symbol (concat (symbol-name (car args)) "-arg")))
        (reject-param (make-symbol (concat (symbol-name (cadr args)) "-arg"))))
    `(promise-later-time ,seconds
                         (lambda (,resolve-param ,reject-param)
                           (cl-labels ((,(car args) (value) (funcall ,resolve-param value))
                                       (,(cadr args) (value) (funcall ,reject-param value)))
                             ,@body)))))

(defun promisep (promise)
  (promise-obj-p promise))

(defun promise--listen (listener notifier)
  (if (promise-obj-done notifier)
      (promise--kickoff listener)
    (push listener (promise-obj-listeners notifier))))

(defun regardless (promise func &optional with-status)
  "After PROMISE resolves or is rejected, run FUNC.

FUNC will be passed two parameters, the error, if any, from PROMISE,
and the resolved value from PROMISE, if any.

When WITH-STATUS is non-nil, a third argument will be passed to FUNC
describing the exit status of PROMISE, it will be `:rejected' or `:resolved'.

A new promise is returned that will resolve to the return value of FUNC,
or reject on an error that occurs in FUNC."
  (let ((obj (make-promise
              (lambda (resolve reject)
                (let ((err (promise-obj-reject promise))
                      (value (promise-obj-resolve promise))
                      (status (if (promise-obj-rejected promise)
                                  :rejected
                                :resolved)))
                  (let ((output
                         (apply
                          func
                          (cons err
                                (cons value
                                      (and with-status (list status)))))))
                    (if (promisep output)
                        (regardless output
                                    (lambda (err value)
                                      (if err
                                          (funcall reject err)
                                        (funcall resolve value))))
                      (funcall resolve output))))))))
    (promise--listen obj promise)
    obj))

(defmacro regardless* (promise args &rest body)
  "TODO: write docs."
  (declare (indent defun))
  `(regardless ,promise (lambda ,args ,@body) ,(= (length args) 3)))

(defun then (promise func &optional err-func)
  "After PROMISE resolves run FUNC, or if rejected, run ERR-FUNC.

A new promise is returned that will resolve to the return value of FUNC
or ERR-FUNC, or reject on an error that occurs in the called function."
  (declare (indent defun))
  (let ((obj (make-promise
              (lambda (resolve reject)
                (let ((err (promise-obj-reject promise))
                      (value (promise-obj-resolve promise)))
                  (let ((output
                         (if (promise-obj-rejected promise)
                             (if err-func
                                 (apply err-func (list err))
                               (rejected-promise err))
                           (if func
                               (apply func (list value))
                             (resolved-promise value)))))
                    (if (promisep output)
                        (regardless output
                                    (lambda (err value)
                                      (if err
                                          (funcall reject err)
                                        (funcall resolve value))))
                      (funcall resolve output))))))))
    (promise--listen obj promise)
    obj))

(defmacro then* (promise args &rest body)
  "TODO: write docs."
  (declare (indent defun))
  `(then ,promise (lambda ,args ,@body)))

(defun on-error (promise err-func)
  "Handle a rejected or erroring PROMISE with ERR-FUNC.

If PROMISE rejects or encounters an error, apply ERR-FUNC
with one arg, the error or rejected value of PROMISE.

    (on-error prom my-err-handler)

    is equivalent to:

    (then prom nil my-err-handler)"
  (then promise nil err-func))

(defmacro on-error* (promise args &rest body)
  "Handle a rejected PROMISE using ARGS and BODY.

This is a convenience wrapper for `on-error'.

    (on-error* promise (err) (message err))

    is equivalent to:

    (on-error promise (lambda (err) (message err)))"
  (declare (indent defun))
  `(on-error ,promise (lambda ,args ,@body)))

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
            (print value))) ; prints '(6)"
  (lambda (&rest args)
    (promise
     (lambda (resolve reject)
       (ignore reject)
       (let* ((output-value nil)
              (callback
               (lambda (&rest cbargs)
                 (funcall resolve (append (list output-value) cbargs)))))
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
  (declare (indent defun))
  `(promise-async (lambda () ,@body)))

(defun promise-all (promises)
  (let ((promises
         (mapcar
          (lambda (p)
            (if (promise-obj-p p)
                p
              (resolved-promise p)))
          promises)))
    (promise
     (lambda (resolve reject)
       (let ((i 0))
         (dolist (prom promises)
           (regardless prom
                       (lambda (err val status)
                         (if (eql status :rejected)
                             (funcall reject err)
                           (when (-all-p (lambda (p) (promise-obj-resolved p)) promises)
                             (funcall resolve (mapcar
                                               (lambda (p) (promise-obj-resolve p))
                                               promises)))))
                       t)
           (incf i)))))))

;;;###autoload
(defmacro chain (&rest args)
  "Easy promise chaining for ARGS.

This is just a alias for `->'."
  (declare (indent 1))
  `(-> ,@args))

(provide 'promises)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; promises.el ends here
