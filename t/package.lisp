#|
  This file is a part of pddl.scheduler project.
|#

(in-package :cl-user)
(defpackage pddl.scheduler-test
  (:use :cl
	:pddl-test
	:alexandria
	:pddl
	:iterate
	:repl-utilities
	:plan-optimizer
        :pddl.scheduler
	:optima
	:fiveam)
  (:shadow :fail :maximize :minimize))
(in-package :pddl.scheduler-test)

(def-suite :pddl.scheduler :in :pddl)
(in-suite :pddl.scheduler)

;; blah blah blah.

