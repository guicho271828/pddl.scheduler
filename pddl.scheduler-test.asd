#|
  This file is a part of pddl.scheduler project.
|#

(defsystem pddl.scheduler-test
  :author ""
  :license ""
  :depends-on (:alexandria
               :iterate
               :optima
               :fiveam
               :pddl
	       :pddl.scheduler)
  :components ((:module "t"
                :components
                ((:file :package)
                 (:file :minimum-slack))))
  :perform (load-op :after (op c) 
		    (eval (read-from-string "(fiveam:run! :pddl.scheduler)"))
		    (asdf:clear-system c)))
