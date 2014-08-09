#|
  This file is a part of pddl.scheduler project.
|#

(defsystem pddl.scheduler
  :version "0.1"
  :author ""
  :license ""
  :depends-on (:pddl
               :guicho-utilities
               :iterate
               :alexandria
               :optima
               :cl-syntax-annot)
  :components ((:file :package :pathname "src/package")
	       (:module "src"
		:depends-on (:package)
                :components
                ((:file :interface)
                 (:file :timed-states)
                 (:file :minimum-slack)
		 (:file :graphical-utils))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op pddl.scheduler-test))))

(defmethod asdf:perform ((op asdf:test-op)
			 (system (eql (asdf:find-system :pddl.scheduler))))
  (funcall (find-symbol "RUN!" (find-package :fiveam)) :pddl.scheduler)
  t)
