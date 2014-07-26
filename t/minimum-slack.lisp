(in-package :pddl.scheduler-test)
(in-suite :pddl.scheduler)

;; sequencial plan
;; #(0 3 3 9 14 15 31 32 37 38 
;;   42 43 49 49 55 60 63 63 66
;;   71 72 78 79 84 85 100 101 101)


(test easiest
  (finishes
    (define (domain make)
      (:requirements :strips :action-cost)
      (:types machine step product)
      (:predicates (available ?m - machine)
                   (use ?m - machine ?s - step)
                   (notusing ?p - product)
                   (using ?m - machine ?p - product)
                   (made ?p - product ?step - step))
      (:functions (total-cost)
                  (span ?p - product ?s - step))
      (:action start
	       :parameters (?p - product ?s - step ?m - machine)
	       :precondition (and (available ?m) (use ?m ?s) (notusing ?p))
	       :effect (and (using ?m ?p)
                            (not (notusing ?p))
                            (not (available ?m))
                            (increase (total-cost) 1)))
      (:action make
	       :parameters (?p - product ?s1 ?s2 - step ?m - machine)
	       :precondition (and (using ?m ?p) (made ?p ?s1))
	       :effect (and (not (made ?p ?s1)) (made ?p ?s2)
                            (increase (total-cost) (span ?p ?s2))))
      (:action end
	       :parameters (?p - product ?s - step ?m - machine)
	       :precondition (and (using ?m ?p) (made ?p ?s))
	       :effect (and (not (using ?m ?p))
                            (notusing ?p)
                            (available ?m)
                            (increase (total-cost) 1)))))
  (finishes
    (define (problem makep)
      (:domain make)
      (:objects m1 m2 - machine p1 p2 - product s0 s1 s2 - step)
      (:init (made p1 s0)
             (made p2 s0)
             (notusing p1)
             (notusing p2)
             (available m1)
             (available m2)
             (use m1 s1)
             (use m2 s2)
             (= (total-cost) 0)
             (= (span p1 s0) 0)
             (= (span p1 s1) 3)
             (= (span p1 s2) 5)
             (= (span p2 s0) 0)
             (= (span p2 s1) 2)
             (= (span p2 s2) 3))
      (:goal (and (made p1 s2) (made p2 s2)))
      (:metric minimize (total-cost))))
  (let ((*domain* make) (*problem* makep))
    (let* ((actions (parse-plan '((start p1 s1 m1)
                                  (make p1 s0 s1 m1)
                                  (end p1 s1 m1)
                                  ;;
                                  (start p1 s2 m2)
                                  (make p1 s1 s2 m2)
                                  (end p1 s2 m2)
                                  ;; 
                                  (start p2 s1 m1)
                                  (make p2 s0 s1 m1)
                                  (end p2 s1 m1)
                                  ;; 
                                  (start p2 s2 m2)
                                  (make p2 s1 s2 m2)
                                  (end p2 s2 m2))))
           (plan (pddl-plan :actions actions))
           (env (pddl-environment :plan plan))
           (last-env (simulate-plan env)))
      (is (= 21 (cost last-env)))
      (is-true (goal-p makep (states last-env)))

      (let (sc)
        (finishes
          (setf sc (reschedule plan :minimum-slack :verbose t))
          (print-timed-action-graphically sc))
        (is (= 17 (timed-state-time (timed-action-end (lastcar sc)))))))))


;; (test build-schedule
;;   (let (schedule)
;;     (finishes
;;       (setf schedule (reschedule cell-assembly-model2a-1-1 :minimum-slack))
;;       (print-timed-action-graphically
;;        schedule
;;        *standard-output*))
;;     ;; (is (= 172 (timed-state-time (timed-action-end (lastcar schedule)))))
;;     ))

;; (test filtering
;;   (finishes
;;     (filter-schedule
;;      (reschedule cell-assembly-model2a-2-1 :minimum-slack)
;;      :objects 'b-0)))

;; ;; regression test
;; (test rover
;;   (finishes
;;     (reschedule roverprob03-3 :minimum-slack)))

