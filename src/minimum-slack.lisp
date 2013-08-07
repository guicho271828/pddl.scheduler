(in-package :pddl.scheduler)
(use-syntax :annot)

;; implement minimum slack scheduler, a kind of scheduler based on the
;; greedy algorithm.

(defgeneric reschedule (plan algorhythm))

@export
(defun %get-costs (plan)
  (let ((cost 0)
	(costs (make-array 5 :fill-pointer 0 :adjustable t)))
    (with-simulating-plan (env (pddl-environment :plan plan))
      (let ((new-cost (cost env)))
	(vector-push-extend (- new-cost cost) costs)
	(setf cost new-cost)))
    costs))
  
@export 'timed-action

(defstruct (timed-action
	     (:constructor timed-action (action duration)))
  action
  duration)

@export
(defun %collect-timed-actions (plan)
  (iter (for action in-vector (actions plan))
	(for cost in-vector (%get-costs plan))
	(collecting (timed-action action cost))))

;; (defmethod reschedule ((plan pddl-plan)
;; 		       (algorhythm (eql :minimum-slack)))
  

