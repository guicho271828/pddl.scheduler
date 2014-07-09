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
  :depends-on (:pddl
	       :pddl.scheduler
	       :pddl.instances
               :pddl.instances.rover
               :repl-utilities
               :pddl-test)
  :components ((:module "t"
                :components
                ((:file :package)
                 (:file :minimum-slack))))
  :perform (load-op :after (op c) 
		    (eval (read-from-string "(fiveam:run! :pddl.scheduler)"))
		    (asdf:clear-system c)))
