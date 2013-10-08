(in-package :pddl.scheduler)
(use-syntax :annot)
(package-optimize-setting)
;; implement minimum slack scheduler, a kind of scheduler based on the
;; greedy algorithm.

(defvar *rescheduler-verbosity* nil)

@export
(defgeneric reschedule (plan algorhythm &key verbose))
(defmethod reschedule ((plan pddl-plan)
                       (algorhythm (eql :minimum-slack)) &key verbose)
  (let ((*rescheduler-verbosity* verbose))
    (%build-schedule plan)))


@export 'timed-state
@export 'timed-state-time
@export 'timed-state-state
@export '(action state time)
(defstruct (timed-state
             (:constructor timed-state (action state time)))
  (action nil :type pddl-actual-action) ; the action just before the state
  (state nil :type list)
  (time nil :type number))

@export 'timed-action
@export 'timed-action-action
@export 'timed-action-start
@export 'timed-action-duration
@export 'timed-action-end
@export 'timed-action-successor-state
@export '(action 
          start
          duration
          end)
(defstruct (timed-action
             (:constructor timed-action (action 
                                         start
                                         duration
                                         end)))
  (action nil :type pddl-actual-action)
  (start nil :type timed-state)
  (duration nil :type number)
  (end nil :type timed-state))

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
  (when *rescheduler-verbosity*
    (format t "~%inserted new action with t = [~a,~a] ,dt = ~a ."
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
(defun sequencial-tss (plan)
  (let* ((cost 0)
         (aa (first-elt (actions plan))) ; initial action
         (ts (timed-state
              aa
              (mapcar #'shallow-copy (init (problem plan)))
              0))
         (timed-states (list ts)))
    (with-simulating-plan (env (pddl-environment :plan plan))
      (let* ((new-cost (cost env))
             (aa (elt (actions plan) (1- (index env)))))
        ;(break+ new-cost duration aa)
        (push (timed-state aa (states env) new-cost) timed-states)
        (setf cost new-cost)))
    (nreverse timed-states)))

(defvar *plan*)

@export
(defun %build-schedule (*plan*)
  @type pddl-plan *plan*
  (let* ((*domain* (domain *plan*))
         (*problem* (problem *plan*))
         (cost 0)
         (timed-states (sequencial-tss *plan*))
         ;; oldest action appears first
         (tas (list (timed-action (first-elt (actions *plan*))
                                  (first timed-states)
                                  0 (first timed-states)))))
    (with-simulating-plan (env (pddl-environment :plan *plan*))
      (let* ((new-cost (cost env))
             (duration (- new-cost cost))
             (aa (elt (actions *plan*) (1- (index env)))))
        (restart-bind ((draw-shrinked-plan
                        (curry #'print-timed-action-graphically
                               (reverse tas) *debug-io*))
                       (draw-shrinked-plan-chronologically
                        (curry #'print-timed-action-graphically
                               (sort-timed-actions tas) *debug-io*)))
          (multiple-value-bind (new-timed-states new-timed-action)
              (%insert-state
               aa duration
               (remove-if
                (lambda (ts)
                  (eq aa (timed-state-action ts)))
                timed-states))
            (check-timed-action new-timed-action)
            (setf timed-states
                  (stable-sort new-timed-states
                               #'< :key #'timed-state-time))
            (push new-timed-action tas)))
        (setf cost new-cost)))
    (reverse tas)))

;; (when *rescheduler-verbosity*
;;   (format t "~%~%Sequencial plan No.~a" (1- (index env)))
;;   (print aa)
;;   (print (action (domain aa) aa))
;;   (format t "~%current action length: ~a" (length aactions)))

;; recursive version
(defun %insert-state (aa duration timed-states)
  (%insert-state-rec aa duration timed-states nil))

(defun remove-fst (states)
  (remove-if (of-type 'pddl-function-state)
             states))

(defun %insert-state-rec (aa duration rest acc)
  (match rest
    ((list* (and ts (timed-state state time)) rest2)
     (if (applicable state aa)
         (if rest2
             (%insert-state-inner
              aa duration ts (+ time duration) rest2 (cons ts acc))
             (%insert-state-finish aa duration ts (cons ts acc)))
         (%insert-state-rec aa duration rest2 (cons ts acc))))
    (nil (%debug-insert-failure aa rest acc))))

(defun %insert-state-inner (aa duration earliest end rest acc)
  (ematch rest
    ((list* (and ts2 (timed-state state time)) rest2)
     (cond
       ((<= time end)
        ;; checks during the time span
        (if (applicable state aa)
            (if rest2
                (%insert-state-inner aa duration earliest end rest2 (cons ts2 acc))
                (%insert-state-mergedto aa earliest duration (cons ts2 acc)))
            (%insert-state-rec aa duration rest2 (cons ts2 acc))))
       ((< end time)
        (if-let ((merged (%check-after-end-time aa end rest acc)))
          ;; if the action is applicable to the all states
          ;; after the finishing time of AA, then AA can be merged fast-forward.
          ;; Apply AA to the last state.
          (%insert-state-merge aa duration earliest merged acc)
          (%insert-state-rec aa duration rest acc)))))))

;;                         |the only applicable state
;; -x---------------x------*---(a)
;; -x---------------x------*----a
(defun %insert-state-finish (aa duration ts acc)
  (when *rescheduler-verbosity*
    (format t "~%inserted a state with no conflict."))
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
(defun %insert-state-merge (aa duration earliest merged before)
  (when *rescheduler-verbosity*
    (format t "~%inserted a state, rebasing the states t >= ~a to it."
            (timed-state-time (car before))))
  (values
   (reverse
    (revappend merged before))
   (timed-action aa earliest duration (car merged))))

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
(defun %check-after-end-time (aa end rest acc)
  ;; check the states after the time span.
  ;; returns a list of timed-state in the chlonological order.
  (assert (applicable (timed-state-state (car acc)) aa))
  (assert (<= (timed-state-time (car acc))
              (timed-state-time (car rest))))
  (let* ((merged-next (apply-actual-action
                       aa (timed-state-state (car acc))))
         (new (timed-state aa merged-next end))
         (merged nil))
    (when (every
           (lambda (ts)
             (match ts
               ((timed-state action time)
                (assert (<= end time))
                (when (applicable merged-next action)
                  (setf merged-next (apply-actual-action action merged-next))
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

(defun %debug-insert-failure (aa rest acc)
  (do-restart ((describe-checked
                (named-lambda describer (n)
                  (let ((state (timed-state-state (nth n (append rest acc)))))
                    (format t "~%~w is ~:[not~;~] applicable to~%~w~
                                  ~%because of:~%"
                            aa (applicable state aa) (remove-fst state))
                    (handler-bind 
                        ((assignment-error (rcurry #'describe *debug-io*))
                         (negative-condition-matched (rcurry #'describe *debug-io*))
                         (state-not-found (rcurry #'describe *debug-io*)))
                      (applicable state aa))))
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
             (path *plan*)
             (path *problem*)
             aa 
             (mapcar fn rest)
             (mapcar fn acc)))))

