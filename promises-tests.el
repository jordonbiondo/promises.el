(defun wait-for-promise (promise)
  (with-timeout (5 (error "Promise timeout"))
    (while (not (promise-obj-done promise))
      (sit-for .05))))

(ert-deftest promises-should-resolve ()
  :tags '("promises")
  (let ((prom
         (promise
          (lambda (resolve reject)
            (funcall resolve 'foo)))))
    (should (promise-obj-resolved prom))))

(ert-deftest promises-should-resolve-values ()
  :tags '("promises")
  (let ((prom
         (promise
          (lambda (resolve reject)
            (funcall resolve 'foo)))))
    (should (equal 'foo (promise-obj-resolve prom)))))


(ert-deftest promises-should-reject ()
  :tags '("promises")
  (let ((prom
         (promise
          (lambda (resolve reject)
            (funcall reject 'foo)))))
    (should (promise-obj-rejected prom))))

(ert-deftest promises-should-reject-values ()
  :tags '("promises")
  (let ((prom
         (promise
          (lambda (resolve reject)
            (funcall reject 'foo)))))
    (should (equal 'foo (promise-obj-reject prom)))))

(ert-deftest promises-should-resolve-synchronously ()
  :tags '("promises")
  (should (eql 5 (promise-obj-resolve (resolved-promise 5)))))

(ert-deftest promises-should-reject-synchronously ()
  :tags '("promises")
  (should (eql 5 (promise-obj-reject (rejected-promise 5)))))

(ert-deftest promises-should-resolve-asynchronously ()
  :tags '("promises")
  (let ((promise (promise-later* (ok nope) (ok "Hello"))))
    (should (not (promise-obj-done promise)))
    (wait-for-promise promise)
    (should (promise-obj-done promise))
    (should (equal (promise-obj-resolve promise) "Hello"))))

(ert-deftest promises-should-reject-asynchronously ()
  :tags '("promises")
  (let ((promise (promise-later* (ok nope) (nope "Hello"))))
    (should (not (promise-obj-done promise)))
    (wait-for-promise promise)
    (should (promise-obj-done promise))
    (should (equal (promise-obj-reject promise) "Hello"))))


(ert-deftest then-should-work ()
  :tags '("promises")
  (let* ((some-value)
         (promise (then
                    (promise-later* (ok nope) (ok "Hello"))
                    (lambda (val) (setq some-value "foo")))))
    (should (not (equal some-value "foo")))
    (wait-for-promise promise)
    (should (equal some-value "foo"))))



(provide 'promises-tests)

