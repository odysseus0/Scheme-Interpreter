(define r
  (lambda ()
    (begin
      (load "A13-test-code.ss")
      (r)
      (load "A14-test-code.ss")
      (r)
      (load "A16-test-code.ss")
      (r))))
