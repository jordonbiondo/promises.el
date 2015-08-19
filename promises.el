;;; promises.el --- Promises -*- lexical-binding: t; -*-

(defun promise--resolve(prom val)
  (puthash :resolve val prom)
  (promise--notify prom))

(defun promise--reject(prom val)
  (puthash :reject val prom)
  (promise--notify prom))

(defun promise--notify (prom)
  (let ((listeners (gethash :listeners prom)))
    (dolist (listener listeners)
      (promise--kickoff listener))))

(defun promise--kickoff (prom)
  (puthash :timer (run-with-timer
                   0 nil
                   (lambda ()
                     (apply (gethash :perform prom) nil)))
           prom))

(defun make-promise (func)
  (let ((obj (make-hash-table :test 'equal)))
    (let ((resolve (apply-partially 'promise--resolve obj))
          (reject (apply-partially 'promise--reject obj)))
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
    (puthash :timer (run-with-timer
                     0 nil
                     (lambda ()
                       (apply (gethash :perform obj) nil)))
             obj)
    obj))

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
                  (funcall resolve (apply func (list err value))))))))
    (push obj (gethash :listeners promise))
    obj))

(defun resolved-promise (val)
  (promise (lambda (resolve reject) (funcall resolve val))))

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
       (let* ((output-value nil)
              (callback (lambda (&rest cbargs) (funcall resolve (append (list output-value) cbargs)))))
         (let ((args (if (and n (< n (length args)))
                         (if (zerop n)
                             (append (list callback) args)
                           (push callback (cdr (nthcdr (1- n) args)))
                           args)
                       (append args (list callback)))))
           (setq output-value (apply func args))))))))
