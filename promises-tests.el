
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


(provide 'promises-tests)

