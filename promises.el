;;; promises.el --- Promises -*- lexical-binding: t; -*-

(defun promise--resolve(prom val)
  (puthash :resolve val prom)
  (puthash :done t prom)
  (promise--notify prom))

(defun promise--reject(prom val)
  (puthash :reject val prom)
  (puthash :done t prom)
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
  (let ((obj (make-hash-table :test 'equal)))
    (let ((resolve (apply-partially 'promise--resolve obj))
          (reject (apply-partially 'promise--reject obj)))
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

(defun delay (func)
  (let ((p (make-promise func)))
    (puthash :delay 0 p)
    (promise--kickoff p)
    p))

(defun delay-time (seconds func)
  (let ((p (make-promise func)))
    (puthash :delay seconds p)
    (promise--kickoff p)
    p))

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
