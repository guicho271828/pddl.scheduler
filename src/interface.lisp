(in-package :pddl.scheduler)
(use-syntax :annot)

;; defines API

(defvar *rescheduler-verbosity* nil)

@export
(defgeneric reschedule (plan algorhythm &key verbose))
(defmethod reschedule ((plan pddl-plan)
                       (algorhythm (eql :minimum-slack)) &key verbose)
  (let ((*rescheduler-verbosity* verbose))
    (%build-schedule plan)))

