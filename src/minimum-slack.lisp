(in-package :pddl.scheduler)
(use-syntax :annot)

;; implement minimum slack scheduler, a kind of scheduler based on the
;; greedy algorithm.

(defgeneric reschedule (plan algorhythm))

;; @export
;; (defun %get-costs (plan)
;;   (let ((cost 0)
;; 	(costs (make-array 5 :fill-pointer 0 :adjustable t)))
;;     (with-simulating-plan (env (pddl-environment :plan plan))
;;       (let ((new-cost (cost env)))
;; 	(vector-push-extend new-cost costs)
;; 	(setf cost new-cost)))
;;     costs))

@export 'timed-action
@export 'timed-action-action
@export 'timed-action-start
@export 'timed-action-duration
@export 'timed-action-end
@export 'timed-action-successor-state


(defstruct (timed-action
	     (:constructor timed-action (action 
					 start
					 duration
					 end)))
  action
  start
  duration
  end)

(defstruct (timed-state
	     (:constructor timed-state (state time)))
  state time)


@export
(defun print-timed-action-graphically (ta s)
  (write-char #\Newline s)
  (multiple-value-bind (quotient remainder)
      (floor (timed-action-start ta) 50)
    (pprint-logical-block
	(s nil
	   :per-line-prefix
	   (if (plusp (timed-action-duration ta))
	       (concatenate
		'string
		(write-to-string (* quotient 50))
		(make-string remainder
			     :initial-element #\Space)
		"|"
		(make-string (1- (timed-action-duration ta))
			     :initial-element #\-)
		"|")
	       (concatenate
		'string
		(write-to-string (* quotient 50))
		(make-string remainder
			     :initial-element #\Space)
		"|")))
      (write (timed-action-action ta) :stream s))))

;; @export
;; (defun %collect-timed-actions (plan)
;;   (iter (for action in-vector (actions plan))
;; 	(for cost in-vector (%get-costs plan))
;; 	(for pcost previous cost initially 0)
;; 	(collecting
;; 	 (timed-action action pcost (- cost pcost) cost))))

(defun conflict-p (aa1 aa2)
  aa1 aa2
  nil)


@export
(defun %collect-timed-actions (plan)
  (iter (for action in-vector (actions plan))
	(for cost in-vector (%get-costs plan))
	(collecting (timed-action action cost))))

;; (defmethod reschedule ((plan pddl-plan)
;; 		       (algorhythm (eql :minimum-slack)))
  
;; parallel-plan : 
