(in-package :pddl.scheduler)
(use-syntax :annot)
(package-optimize-setting)
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

(defun check-timed-action (new-timed-action)
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
	  new-timed-action))

(defun sort-timed-actions (aactions)
  (stable-sort
   (copy-seq aactions)
   (lambda (ta1 ta2)
     (cond 
       ((< (timed-state-time (timed-action-start ta1))
	   (timed-state-time (timed-action-start ta2))) t)
       ((> (timed-state-time (timed-action-start ta1))
	   (timed-state-time (timed-action-start ta2))) nil)
       ((= (timed-state-time (timed-action-start ta1))
	   (timed-state-time (timed-action-start ta2)))
	(< (timed-state-time (timed-action-end ta1))
	   (timed-state-time (timed-action-end ta2))))))))

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
	(restart-bind ((draw-shrinked-plan
			(lambda ()
			  (print-timed-action-graphically
			   (reverse aactions) *debug-io*)))
		       (draw-shrinked-plan-chronologically
			(lambda ()
			  (print-timed-action-graphically
			   (sort-timed-actions aactions)
			   *debug-io*))))
	  (multiple-value-bind (new-timed-states new-timed-action)
	      (%insert-state aa duration timed-states)
	    (check-timed-action new-timed-action)
	    (setf timed-states
		  (stable-sort new-timed-states
			       #'< :key #'timed-state-time))
	    (push new-timed-action aactions)))
	(format t "~%current action length: ~a" (length aactions))
	(setf cost new-cost)))
    (sort-timed-actions aactions)))

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
	      aa duration ts (+ time duration) rest2 (cons ts acc))
	     (%insert-state-finish aa duration ts (cons ts acc)))
	 (%insert-state-rec aa duration rest2 (cons ts acc))))
    (nil (%debug-insert-failure aa rest acc))))

(defun %insert-state-inner (aa duration earliest end rest acc)
  (ematch rest
    ((list* (and ts2 (timed-state state time)) rest2)
     (if (<= time end)
	 ;; checks during the time span
	 (if (appliable state aa)
	     (if rest2
		 (%insert-state-inner aa duration earliest end rest2 (cons ts2 acc))
		 (%insert-state-mergedto aa earliest duration (cons ts2 acc)))
	     (%insert-state-rec aa duration rest2 (cons ts2 acc)))
	 (if-let ((merged (%check-after-end-time aa duration rest acc)))
	   ;; if the action is applicable to the all states
	   ;; after the finishing time of AA, then AA can be merged fast-forward.
	   ;; Apply AA to the last state.
	   (%insert-state-merge aa duration merged acc)
	   (%insert-state-rec aa duration rest acc))))))

;;                         |the only applicable state
;; -x---------------x------*---(a)
;; -x---------------x------*----a
(defun %insert-state-finish (aa duration ts acc)
  (format t "~%inserted a state with no conflict.")
  (let ((new
	 (timed-state
	  aa
	  (apply-actual-action aa (timed-state-state ts))
	  (+ (timed-state-time ts) duration))))
    (values
     (nreverse
      (cons new acc))
     (timed-action aa ts duration new))))

;; in %insert-state-inner,
;;
;;    acc<<|               |time
;; --x-----*---------------?------?
;;         *----(a)        |>>rest
;;               |end
;;
;; if all ? can be reached
;; by applying the same action sequence after * to (a)
;; then it is feasible to insert (a) between * and ?
;;
;; before<<|               |time
;; --x-----*        /------x------x
;;         *-----a-/       |>>merged
(defun %insert-state-merge (aa duration merged before)
  (format t "~%inserted a state, rebasing the states t >= ~a to it."
	  (timed-state-time (car before)))
  (values
   (reverse
    (revappend merged before))
   (timed-action aa (car before) duration (car merged))))

(defun apply-action-timed-state (aa ts duration)
  (timed-state
   aa
   (apply-actual-action aa (timed-state-state ts))
   (+ duration (timed-state-time ts))))

;; in %insert-state-inner,
;;
;;            |time
;;    before<<|(car acc)
;; --x--*--*--*
;;      *-------(a)
;;               |end
;;
;;    before<<|
;; --x--*--*--*
;;            +-(a)
;;
;; however in the case below,
;; merging fails and abort the insertion.
;; back to %insert-state-rec.
;;
;;         |time
;; before<<|  
;; --x--*--x--(rest)
;;      *-------(a)
;;               |end
;; 
(defun %insert-state-mergedto (aa earliest duration before)
  (let* ((last (car before))
	 (new (apply-action-timed-state
	       aa last duration)))
    (setf (timed-state-time new)
	  (+ (timed-state-time earliest) duration))
    (values
     (reverse
      (cons new before))
     (timed-action aa earliest duration new))))

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

