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
	     (:constructor timed-state (action state time)))
  action state time)

@export
(defun print-timed-action-graphically (ta s)
  (write-char #\Newline s)
  (pprint-logical-block
      (s nil
	 :per-line-prefix
	 (ematch ta
	   ((timed-action (start (timed-state time)) duration)
	    (multiple-value-bind (quotient remainder) (floor time 50)
	      (if (plusp duration)		
		  (format nil "~a~a|~a|" 
			  (* quotient 50)
			  (make-string remainder
				       :initial-element #\Space)
			  (make-string (1- (timed-action-duration ta))
				       :initial-element #\-))
		  (format nil "~a~a|"
			  (* quotient 50)
			  (make-string remainder
				       :initial-element #\Space)))))))
    (write (timed-action-action ta) :stream s)))

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
  (let* ((cost 0)
	 (aa (first-elt (actions plan)))
	 (ts (timed-state
	      aa
	      (mapcar #'shallow-copy (init (problem plan)))
	      0))
	 (timed-states (list ts))
	  ;; oldest action appears first
	 (aactions (list (timed-action aa ts 0 ts))))
    (with-simulating-plan (env (pddl-environment :plan plan))
      ;; (pprint-logical-block (*standard-output*
      ;; 			     nil :per-line-prefix 
      ;; 			     (format nil "index: ~a " (index env)))
      ;;)	
      (let* ((new-cost (cost env))
	     (duration (- new-cost cost))
	     (aa (elt (actions plan) (1- (index env)))))
	(format t "~%~%Sequencial plan No.~a" (1- (index env)))
	(print aa)
	(print (action (domain aa) aa))
	(multiple-value-bind (new-timed-states new-timed-action)
	    (%insert-state aa duration timed-states)
	  (format t "~%inserted new action with t = [~a,~a] ,dt = ~a ."
		  (timed-state-time 
		   (timed-action-start new-timed-action))
		  (timed-state-time 
		   (timed-action-end new-timed-action))
		  (timed-action-duration new-timed-action))

	  (assert (<= (timed-state-time 
		       (timed-action-start new-timed-action))
		      (timed-state-time 
		       (timed-action-end new-timed-action)))
		  nil "illegal start/end time: [~a,~a]~%in:~%~w"
		  (timed-state-time 
		   (timed-action-start new-timed-action))
		  (timed-state-time 
		   (timed-action-end new-timed-action))
		  new-timed-action)

	  ;; (break+ timed-states new-timed-states)
	  (setf timed-states new-timed-states)
	  (iter (for aas on aactions)
		(when (<= (timed-state-time
			   (timed-action-end (first aas)))
			  (timed-state-time
			   (timed-action-start new-timed-action)))
		  (push new-timed-action (cdr aas))
		  (terminate))))
	(format t "~%current action length: ~a" (length aactions))
	(setf cost new-cost)))					
    (nreverse aactions)))

;; recursive version
(defun %insert-state (aa duration timed-states)
  (%insert-state-rec aa duration timed-states nil))

(defun %insert-state-rec (aa duration rest acc)
  (match rest
    ((list* (and ts (timed-state state time)) rest)
     (if (appliable state aa)
	 (if rest
	     (%insert-state-inner
	      aa duration (+ time duration) rest (cons ts acc))
	     (progn
	       ;(break+)
	       (%insert-state-finish
		aa nil ts duration (+ time duration) (cons ts acc))))
	 (%insert-state-rec aa duration rest (cons ts acc))))
    (nil (error "failed to insert the action to the list! ~%~a~%~a"
		aa acc))))

(defun %insert-state-inner (aa duration end-time rest acc)
  (ematch rest
    ((list* (and ts2 (timed-state state time)) rest2)
     (if (<= time end-time)
	 (if (appliable state aa)
	     (if rest2
		 (%insert-state-inner aa duration end-time rest2 (cons ts2 acc))
		 (%insert-state-finish aa rest2 ts2 duration end-time (cons ts2 acc)))
	     (%insert-state-rec aa duration rest2 (cons ts2 acc)))
	 (if (appliable state aa)
	     (%insert-state-finish aa rest2 (car acc) duration end-time (cons ts2 acc))
	     (%insert-state-rec aa duration rest2 (cons ts2 acc)))))))

(defun %insert-state-finish (aa rest ts duration end-time acc)
  (let ((new (timed-state
	      aa
	      (apply-actual-action aa (timed-state-state ts))
	      (+ (timed-state-time ts) duration))))
    (values
     (nreverse
      (revappend rest (cons new acc)))
     (timed-action aa ts duration new))))

;; (defmethod reschedule ((plan pddl-plan)
;; 		       (algorhythm (eql :minimum-slack)))
  
;; parallel-plan : 
