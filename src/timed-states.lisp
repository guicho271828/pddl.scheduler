(in-package :pddl.scheduler)
(use-syntax :annot)

;; defines timed-state and timed-action

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
