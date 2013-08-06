#|
  This file is a part of pddl.scheduler project.
|#

(in-package :cl-user)
(defpackage pddl.scheduler-test-asd
  (:use :cl :asdf))
(in-package :pddl.scheduler-test-asd)

(defsystem pddl.scheduler-test
  :author ""
  :license ""
  :depends-on (:pddl.scheduler
	       :pddl-test)
  :components ((:module "t"
                :components
                ((:file "pddl.scheduler"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
