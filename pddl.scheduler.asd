#|
  This file is a part of pddl.scheduler project.
|#

(in-package :cl-user)
(defpackage pddl.scheduler-asd
  (:use :cl :asdf))
(in-package :pddl.scheduler-asd)

(defsystem pddl.scheduler
  :version "0.1"
  :author ""
  :license ""
  :depends-on (:pddl
	       :plan-optimizer
               :guicho-utilities
               :iterate
               :alexandria
               :optima
               :cl-syntax-annot
               :anaphora
               :fiveam
               :metatilities)
  :components ((:file :package :pathname "src/package")
	       (:module "src"
		:depends-on (:package)
                :components
                ((:file :minimum-slack)
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
