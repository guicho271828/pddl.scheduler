(in-package :pddl.scheduler)
(use-syntax :annot)

@export
(defun filter-schedule (timed-actions
                        &key
                        (objects nil)
                        (actions nil)
                        (start 0)
                        (end MOST-POSITIVE-FIXNUM))
  (let ((objects (ensure-list objects))
        (actions (ensure-list actions)))
    (remove-if-not
     (lambda (ta)
       (match ta
         ((timed-action
           (action (pddl-ground-action domain problem name parameters))
           (start (timed-state (time start-time)))
           (end (timed-state (time end-time))))
          (and (<= start start-time)
               (<= end-time end)
               (if actions
                   (member
                    (action domain name)
                    (mapcar (lambda (a)
                              (or (action domain a)
                                  (error "no such action!: ~a" a)))
                            actions))
                   t)
               (if objects
                   (intersection
                    parameters
                    (mapcar (lambda (o) (object problem o))
                            objects))
                   t)))))
     timed-actions)))