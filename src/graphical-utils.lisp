(in-package :pddl.scheduler)
(use-syntax :annot)

(defparameter *truncate-width* 250)

@export
(defun print-timed-action-graphically
    (timed-actions &optional (s *standard-output*))
  (dolist (ta (ensure-list timed-actions))
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
              @ignorable quotient remainder
              (if (plusp duration)
                  (format nil "~4@a ~a|~a|"
                          time
                          (make-string remainder
                                       :initial-element #\Space)
                          (make-string (1- (timed-action-duration ta))
                                       :initial-element #\-))
                  (format nil "~4@a ~a|"
                          time
                          (make-string remainder
                                       :initial-element #\Space)))))))
    (let ((*print-pretty* nil))
      (prin1 (timed-action-action ta) s))))

@export
(defun filter-schedule (timed-actions
                        &key
                        (objects nil)
                        (actions nil)
                        (start 0)
                        (end MOST-POSITIVE-FIXNUM))
  "Prints the schedule with certain time window"
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
