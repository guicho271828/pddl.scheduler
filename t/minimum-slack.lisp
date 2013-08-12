(in-package :pddl.scheduler-test)
(in-suite :pddl.scheduler)

;; sequencial plan
;; #(0 3 3 9 14 15 31 32 37 38 
;;   42 43 49 49 55 60 63 63 66
;;   71 72 78 79 84 85 100 101 101)

(test build-schedule
  (finishes
    (mapc (rcurry #'print-timed-action-graphically *standard-output*)
	  (%build-schedule CELL-ASSEMBLY-ANYWHERE-P3-3))))