#|
  This file is a part of pddl.scheduler project.
|#

(in-package :cl-user)
(defpackage pddl.scheduler
  (:use :cl
        :pddl
        :plan-optimizer
        :guicho-utilities
        :iterate
        :alexandria
        :optima
        :cl-syntax
        :anaphora)
  (:shadowing-import-from :iterate :maximize :minimize))
(in-package :pddl.scheduler)

(use-syntax :annot)
;; blah blah blah.

