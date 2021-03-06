(in-package :pddl.scheduler)
(use-syntax :annot)
(package-optimize-setting)
;; implement minimum slack scheduler, a kind of scheduler based on the
;; greedy algorithm.

(defvar *plan*)
(defun draw-handler (tas)
  (print-timed-action-graphically tas *debug-io*))
(defun check-timed-action (new-timed-action)
  (when *rescheduler-verbosity*
    (format t "~%inserted a new action with t = [~a,~a] ,dt = ~a ."
            (timed-state-time 
             (timed-action-start new-timed-action))
            (timed-state-time 
             (timed-action-end new-timed-action))
            (timed-action-duration new-timed-action)))
  (assert (= (+ (timed-action-duration new-timed-action)
                (timed-state-time 
                 (timed-action-start new-timed-action)))
             (timed-state-time 
              (timed-action-end new-timed-action)))
          nil "illegal start/end time: [~a,~a], dt=~a~%in:~%~w"
          (timed-state-time 
           (timed-action-start new-timed-action))
          (timed-state-time 
           (timed-action-end new-timed-action))
          (timed-action-duration new-timed-action)
          new-timed-action))

@export
(defun sort-schedule (schedule)
  (stable-sort
   (copy-seq schedule)
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

(defun sequencial-schedule (plan)
  "Converts a pddl-plan object into a list of timed-state"
  (let* (tss tas (prev-cost 0) prev-ts)
    (with-simulating-plan (env (pddl-environment :plan plan))
      (let* ((cost (cost env))
             (duration (- cost prev-cost))
             (state (states env)))
        (cond
          ((zerop (index env))
           (let* ((ts (timed-state nil state cost))
                  (ta (timed-action nil ts duration ts)))
             (push ts tss)
             (push ta tas)
             (setf prev-cost cost prev-ts ts)))
          (t
           (let* ((action (elt (actions plan) (1- (index env))))
                  (ts (timed-state action state cost))
                  (ta (timed-action action prev-ts duration ts)))
             (push ts tss)
             (push ta tas)
             (setf prev-cost cost prev-ts ts))))))
    (values
     (nreverse tss)
     (nreverse tas))))

(defun %build-schedule (*plan*)
  (let* ((*domain* (domain *plan*))
         (*problem* (problem *plan*)))
    (multiple-value-bind (tss tas) (sequencial-schedule *plan*)
      (restart-bind ((draw-shrinked-plan
                      (lambda () (draw-handler tas)))
                     (draw-shrinked-plan-chronologically
                      (lambda () (draw-handler (sort-schedule tas)))))
        (iter (for ta in (cdr tas))
              (ematch ta
                ((timed-action a _ duration _)
                 (multiple-value-bind (new-tss new-ta)
                     (%insert-state a duration tss)
                   (check-timed-action new-ta)
                   (setf tss (stable-sort new-tss #'< :key #'timed-state-time))
                   (collect new-ta)))))))))


(defun remove-fst (states)
  "Removes function states"
  (remove-if (of-type 'pddl-function-state) states))

;; ga = grounded action

;; recursive version

(defun %insert-state (ga duration timed-states)
  (%insert-state-rec
   ga
   duration
   ;; remove itself -- therefore it should be applicable at some point
   ;; (the point it used to be)
   (remove ga timed-states :key #'timed-state-action)
   nil))

(defun %insert-state-rec (ga duration rest acc)
  (ematch rest
    ((list* (and ts (timed-state _ state time)) rest2)
     (if (applicable state ga)
         (if rest2
             (%insert-state-inner
              ga duration ts (+ time duration) rest2 (cons ts acc))
             (%insert-state-finish ga duration ts (cons ts acc)))
         (%insert-state-rec ga duration rest2 (cons ts acc))))
    (nil (%debug-insert-failure ga rest acc))))

(defun %insert-state-inner (ga duration earliest end rest acc)
  (ematch rest
    ((list* (and ts2 (timed-state _ state time)) rest2)
     (cond
       ((< time end)
        ;; checks during the time span
        (if (applicable state ga)
            (if rest2
                (%insert-state-inner ga duration earliest end rest2 (cons ts2 acc))
                (%insert-state-mergedto ga earliest duration (cons ts2 acc)))
            (%insert-state-rec ga duration rest2 (cons ts2 acc))))
       ((<= end time)
        (if-let ((merged (%check-after-end-time ga end rest acc)))
          ;; if the action is applicable to the all states
          ;; after the finishing time of GA, then GA can be merged fast-forward.
          ;; Apply GA to the last state.
          (%insert-state-merge ga duration earliest merged acc)
          (%insert-state-rec ga duration rest acc)))
       (t (error "What? time: ~a end: ~a" time end))))))

;;                         |the only applicable state
;; -x---------------x------*---(a)
;; -x---------------x------*----a
(defun %insert-state-finish (ga duration ts acc)
  (when *rescheduler-verbosity*
    (format t "~%inserted a state with no conflict."))
  (let ((new
         (timed-state
          ga
          (apply-ground-action ga (timed-state-state ts))
          (+ (timed-state-time ts) duration))))
    (values
     (nreverse
      (cons new acc))
     (timed-action ga ts duration new))))

;; in %insert-state-inner,
;;
;;    acc<<|               |time
;; --x-----*---------------?--?--?--?--?--...
;;         *----(a)        |>>rest
;;               |end
;;
;; if all ? can be reached
;; by applying the same action sequence after * to (a)
;; then it is feasible to insert (a) between * and ?.
;; this is checked by %check-after-end-time.
;;
;; before<<|     |>>merged |time
;; --x-----*     |  /------x------x
;;         *-----a-/
;;
;; this is also possible after some calls to %insert-state-inner:
;;
;;       acc<<|            |time
;; --x-----*--*------------?--?--?--?--?--...
;;         *----(a)        |>>rest
;;               |end
;;
;; the consequense is the same:
;;
;;    before<<|  |>>merged |time
;; --x-----*--*  |  /------x------x
;;            +--a-/
;; 
(defun %insert-state-merge (ga duration earliest merged before)
  (when *rescheduler-verbosity*
    (format t "~%inserted a state, rebasing the states t >= ~a to it."
            (timed-state-time (car before))))
  (values
   (reverse
    (revappend merged before))
   (timed-action ga earliest duration (car merged))))

(defun apply-action-timed-state (ga ts duration)
  (timed-state
   ga
   (apply-ground-action ga (timed-state-state ts))
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
(defun %insert-state-mergedto (ga earliest duration before)
  (let* ((last (car before))
         (new (apply-action-timed-state
               ga last duration)))
    (setf (timed-state-time new)
          (+ (timed-state-time earliest) duration))
    (values
     (reverse
      (cons new before))
     (timed-action ga earliest duration new))))

;;       acc<<|            |time
;; --x-----*--*------------?--?--?--?--?--...
;;         *----(a)        |>>rest
;; earliest|     |end,new
;;
;; if all ? can be reached
;; by applying the same action sequence after * to (a)
;; then it is feasible to insert (a) between * and ?.
;; this is checked by %check-after-end-time.
;;
;; after %check-after-end-time. all ?s are merged  to (a).
;;
;;       acc<<|            |time
;; --x-----*--*-\ /--------?--?--?--?--?--...
;;         *----(a)        |>>rest
;; earliest|     |end,new
;;
(defun %check-after-end-time (ga end rest acc)
  ;; check the states after the time span.
  ;; returns a list of timed-state in the chlonological order.
  (assert (applicable (timed-state-state (car acc)) ga))
  (assert (<= (timed-state-time (car acc))
              (timed-state-time (car rest))))
  (let* ((merged-next
          (if ga
              (apply-ground-action ga (timed-state-state (car acc)))
              (timed-state-state (car acc)))) ;; because it is the initial state
         (new (timed-state ga merged-next end))
         (merged nil))
    (when (every
           (lambda (ts)
             (ematch ts
               ((timed-state action _ time)
                (assert (<= end time))
                (when (applicable merged-next action)
                  (setf merged-next (apply-ground-action action merged-next))
                  (push (timed-state action merged-next time) merged)
                  t))))
           rest)
      (iter (for merged-state in (nreverse merged))
            (for ts in rest)
            (assert (eq (timed-state-action merged-state)
                        (timed-state-action ts)))
            (setf (timed-state-state ts)
                  (timed-state-state merged-state)))
      (cons new rest))))

(defun %debug-insert-failure (ga rest acc)
  (do-restart ((describe-checked
                (named-lambda describer (n)
                  (let ((state (timed-state-state (nth n (append rest acc)))))
                    (format t "~%~w is ~:[not~;~] applicable to~%~w"
                            ga (applicable state ga) state)
                    (handler-bind 
                        ((condition (lambda (c)
                                      (format *debug-io* "~&because of:~%")
                                      (describe c *debug-io*))))
                      (applicable state ga))))
                :interactive-function
                (named-lambda reader ()
                  (format *query-io* "~%enter a number:")
                  (list (read *query-io*)))))
    (let ((fn (let ((i -1))
                (lambda (ts) (list (incf i)
                                   (timed-state-time ts)
                                   (timed-state-action ts))))))
      (error "In plan ~w ~&in problem ~w,~%failed to insert the action to the list!~
               ~%~a~
               ~%not checked yet:~%~:{n: ~w time: ~w action:~w~%~}~
               ~%already checked:~%~:{n: ~w time: ~w action:~w~%~}"
             (ignore-errors (path *plan*))
             (ignore-errors (path *problem*))
             ga 
             (mapcar fn rest)
             (mapcar fn acc)))))

