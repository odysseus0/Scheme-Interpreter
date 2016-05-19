; Author: Shunfan Du, Tengji Zhang

(load "chez-init.ss")

(define load-all
  (lambda ()
    (load "datatypes.ss")
    (load "parser.ss")
    (load "environments.ss")
    (load "interpreter.ss")))

(load-all)
(load-all)

(define l load-all) ; Shortcut for load-all


