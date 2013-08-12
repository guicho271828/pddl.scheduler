(in-package :pddl.scheduler)
(use-syntax :annot)

;; implement minimum slack scheduler, a kind of scheduler based on the
;; greedy algorithm.

(defgeneric reschedule (plan algorhythm))

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

(defmethod print-object ((o timed-action) s)
  (match o
    ((timed-action action duration
		   (start (timed-state time))
		   (end (timed-state (time time2))))
     (format s "(TA ~w :start ~a :end ~a :dt ~a)"
	     action time time2 duration))))

(defmethod print-object ((o timed-state) s)
  (match o
    ((timed-state time action)
     (format s "(TS :time ~w :action ~w)"
	     time action))))

(defparameter *truncate-width* 250)

@export
(defun print-timed-action-graphically
    (tas &optional (s *standard-output*))
  (dolist (ta (ensure-list tas))
    (%print-timed-action-graphically ta s)))

(defun %print-timed-action-graphically (ta s)
  (write-char #\Newline s)
  (pprint-logical-block
      (s nil
	 :per-line-prefix
	 (ematch ta
	   ((timed-action (start (timed-state time)) duration)
	    (multiple-value-bind (quotient remainder)
		(floor time *truncate-width*)
	      (if (plusp duration)		
		  (format nil "~a~a|~a|" 
			  (* quotient *truncate-width*)
			  (make-string remainder
				       :initial-element #\Space)
			  (make-string (1- (timed-action-duration ta))
				       :initial-element #\-))
		  (format nil "~a~a|"
			  (* quotient *truncate-width*)
			  (make-string remainder
				       :initial-element #\Space)))))))
    (let ((*print-pretty* nil))
      (prin1 (timed-action-action ta) s))))

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

	  (setf timed-states (sort new-timed-states
				   #'< :key #'timed-state-time))
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

(defun remove-fst (states)
  (remove-if (of-type 'pddl-function-state)
	     states))

(defun %insert-state-rec (aa duration rest acc)
  (match rest
    ((list* (and ts (timed-state state time)) rest2)
     (if (appliable state aa)
	 (if rest2
	     (%insert-state-inner
	      aa duration (+ time duration) rest2 (cons ts acc))
	     (%insert-state-finish
	      aa duration ts (cons ts acc)))
	 (%insert-state-rec aa duration rest2 (cons ts acc))))
    (nil (%debug-insert-failure aa rest acc))))

(defun %insert-state-inner (aa duration end-time rest acc)
  (ematch rest
    ((list* (and ts2 (timed-state state time)) rest2)
     (if (<= time end-time)
	 ;; checks during the time span
	 (if (appliable state aa)
	     (if rest2
		 (%insert-state-inner aa duration end-time rest2 (cons ts2 acc))
		 (%insert-state-finish aa duration ts2 (cons ts2 acc)))
	     (%insert-state-rec aa duration rest2 (cons ts2 acc)))
	 (if-let ((merged (%check-after-end-time aa duration rest acc)))
	   ;; if the action is applicable to the all states
	   ;; after the finishing time of AA, then AA can be merged fast-forward.
	   ;; Apply AA to the last state.
	   (%insert-state-merge aa duration merged acc)
	   (%insert-state-rec aa duration rest acc))))))

(defun %insert-state-finish (aa duration ts acc)
  ;; called when there is no timed-state after ts.
  ;; apply AA to the last state in the timeline.
  (let ((new
	 (timed-state
	  aa
	  (apply-actual-action aa (timed-state-state ts))
	  (+ (timed-state-time ts) duration))))
    (values
     (nreverse
      (cons new acc))
     (timed-action aa ts duration new))))

(defun %check-after-end-time (aa duration rest acc)
  ;; check the states after the time span.
  ;; returns a list of timed-state in the chlonological order.
  (let* ((merged nil)
	 (prev (car acc))
	 (merged-next (apply-actual-action
		       aa (timed-state-state prev)))
	 (new (timed-state
	       aa
	       merged-next
	       (+ (timed-state-time prev) duration))))
    (push new merged)
    (when (every
	   (lambda (ts)
	     (match ts
	       ((timed-state action (state (place state)))
		(when (appliable merged-next action)
		  (setf state (apply-actual-action action merged-next)
			merged-next state)
		  (push ts merged)
		  t)))) rest)
      (nreverse merged))))



(defun %insert-state-merge (aa duration merged acc)
  (values
   (nreverse
    (revappend merged acc))
   (timed-action aa (car acc) duration (car merged))))

;; (defmethod reschedule ((plan pddl-plan)
;; 		       (algorhythm (eql :minimum-slack)))
  
;; parallel-plan : 

(defun %debug-insert-failure (aa rest acc)
  (do-restart ((describe-checked
		(named-lambda describer (n)
		  (let ((state (timed-state-state (nth n (append rest acc)))))
		    (format t "~%~w is ~:[not~;~] appliable to~%~w~
                                  ~%because of:~%"
			    aa (appliable state aa) (remove-fst state))
		    (handler-bind 
			((assignment-error (rcurry #'describe *debug-io*))
			 (state-not-found (rcurry #'describe *debug-io*)))
		      (appliable state aa))))
		:interactive-function
		(named-lambda reader ()
		  (format *query-io* "~%enter a number:")
		  (list (read *query-io*)))))
    (let ((fn (lambda (ts) (list (timed-state-time ts)
				 (timed-state-action ts)))))
      (error "failed to insert the action to the list!~
               ~%~a~
               ~%not checked yet:~%~:{time: ~w action:~w~%~}~
               ~%already checked:~%~:{time: ~w action:~w~%~}"
	     aa 
	     (mapcar fn rest)
	     (mapcar fn acc)))))