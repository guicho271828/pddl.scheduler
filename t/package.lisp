#|
  This file is a part of pddl.scheduler project.
|#

(in-package :cl-user)
(defpackage pddl.scheduler-test
  (:use :cl
	:alexandria
	:iterate
	:optima
	:fiveam
	:pddl
	:pddl.plan-optimizer
        :pddl.scheduler)
  (:shadow :fail)
  (:shadowing-import-from :pddl  :maximize :minimize))
(in-package :pddl.scheduler-test)

(def-suite :pddl.scheduler)
(in-suite :pddl.scheduler)

;; blah blah blah.

