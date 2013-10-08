(in-package :pddl.scheduler-test)
(in-suite :pddl.scheduler)

;; sequencial plan
;; #(0 3 3 9 14 15 31 32 37 38 
;;   42 43 49 49 55 60 63 63 66
;;   71 72 78 79 84 85 100 101 101)

(test build-schedule
  (finishes
    (print-timed-action-graphically
     (reschedule cell-assembly-model2a-2-1 :minimum-slack)
     *standard-output*)))

(test filtering
  (finishes
    (filter-schedule
     (reschedule cell-assembly-model2a-2-1 :minimum-slack)
     :objects 'b-0)))

(quote #.(load-rovers))

;; regression test
(test rover2435-1
  (finishes
    (reschedule roverprob2435-1 :minimum-slack)))