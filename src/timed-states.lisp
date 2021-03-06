(in-package :pddl.scheduler)
(use-syntax :annot)

;; defines timed-state and timed-action

@export 'timed-state
@export 'timed-state-p
@export 'timed-state-time
@export 'timed-state-state
@export 'timed-state-action
@export '(action state time)
(defstruct (timed-state
             (:constructor timed-state (action state time)))
  (action nil :type (or null pddl-ground-action)) ; the action just before the state
  (state nil :type list)
  (time 0 :type number))

(defpattern timed-state (action state time)
  `(structure timed-state-
              (action ,action)
              (state ,state)
              (time ,time)))

@export 'timed-action
@export 'timed-action-p
@export 'timed-action-action
@export 'timed-action-start
@export 'timed-action-duration
@export 'timed-action-end
@export '(action start duration end)
(defstruct (timed-action
             (:constructor timed-action (action 
                                         start
                                         duration
                                         end)))
  (action nil :type (or null pddl-ground-action))
  (start nil :type timed-state)
  (duration nil :type number)
  (end nil :type timed-state))

(defpattern timed-action (action start duration end)
  `(structure timed-action-
              (action ,action)
              (start ,start)
              (duration ,duration)
              (end ,end)))

(defmethod print-object ((o timed-action) s)
  (match o
    ((timed-action action
                   (timed-state _ _ time)
                   duration
                   (timed-state _ _ time2))
     (format s "(TA ~w :t1 ~a :t2 ~a :dt ~a)"
             action time time2 duration))))

(defmethod print-object ((o timed-state) s)
  (match o
    ((timed-state action _ time)
     (format s "(TS :t ~w :action ~w)"
             time action))))
