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
(defun %build-schedule (plan)
  @type pddl-plan plan
  (let ((cost 0)
	(timed-states
	 (list (timed-state
		(mapcar #'shallow-copy
			(init (problem plan)))
		0)))
	(aactions
	 (list (timed-action
		(first-elt (actions plan))
		0 0 0)))) ;; oldest action appears first
    (with-simulating-plan (env (pddl-environment :plan plan))
      (pprint-logical-block (t
			     nil :per-line-prefix 
			     (format nil "index: ~a " (index env)))
	(let* ((new-cost (cost env))
	       (duration (- new-cost cost))
	       (aa (elt (actions plan) (index env))))
	  (print aa)
	  (print (action (domain aa) aa))
	  (write-char #\Newline)
	  (iter (for aa-rest first aactions
		     then (cdr aa-rest))
		(for i from 0)
		(when (endp aa-rest) (terminate))
		(for old-a = (car aa-rest))
		(for states = (timed-action-successor-states old-a))
		(when (and (appliable states aa)
			   (or (null (second aa-rest))
			       (not (conflict-p
				     (timed-action-action (second aa-rest))
				     aa))))
		  (format t "~%inserting new state after ~ath action"
			  i)
		  (push (timed-action
			 aa
			 (timed-action-end old-a)
			 duration
			 (+ (timed-action-end old-a) duration)		       
			 (apply-actual-action aa states))
			(cdr aa-rest))
		  (leave))
		(finally
		 ;; action is supposed to be inserted to the list
		 ;; because the last state always matches the precondition
		 (warn "failed to insert the action to the list!")))
	  (format t "current action length: ~a~%" (length aactions))
	  (setf cost new-cost))))
    aactions))
    

;; (defmethod reschedule ((plan pddl-plan)
;; 		       (algorhythm (eql :minimum-slack)))
  
;; parallel-plan : 
