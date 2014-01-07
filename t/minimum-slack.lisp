(in-package :pddl.scheduler-test)
(in-suite :pddl.scheduler)

;; sequencial plan
;; #(0 3 3 9 14 15 31 32 37 38 
;;   42 43 49 49 55 60 63 63 66
;;   71 72 78 79 84 85 100 101 101)

(test build-schedule
  (let (schedule)
    (finishes
      (setf schedule (reschedule cell-assembly-model2a-1-1 :minimum-slack))
      (print-timed-action-graphically
       schedule
       *standard-output*))
    ;; (is (= 172 (timed-state-time (timed-action-end (lastcar schedule)))))
    ))

(test filtering
  (finishes
    (filter-schedule
     (reschedule cell-assembly-model2a-2-1 :minimum-slack)
     :objects 'b-0)))

;; regression test
(test rover
  (finishes
    (reschedule roverprob03-3 :minimum-slack)))

