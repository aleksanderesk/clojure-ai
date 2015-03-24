;;;; Name: Aleksander Eskilson
;;;; KUID: 2373732
;;;; Email: aeskilson@ku.edu
;;;; Descr: Main execution function
(ns deduce.core
  (:gen-class)
  (:require [deduce.resolve :as res]
            [deduce.algo :as algo]))

;;; Axioms and Start states
(def axioms1
  '(((Traitor ?x) (¬(American ?x)) (¬(Weapon ?y)) (¬(Sells ?x ?y ?z)) (¬(Hostile ?z)))
    ((Enemy Nono America))
    ((Owns Nono M1))
    ((Missile M1))
    ((Sells West ?x Nono) (¬(Missile ?x)) (¬(Owns Nono ?x)))
    ((American West))
    ((Weapon ?x) (¬(Missile ?x)))
    ((Hostile ?x) (¬(Hostile (Mother-of ?x))))
    ((Hostile ?x) (¬(Enemy ?x America)))
    ((Hostile ?x) (¬(Hostile (Father-of ?x))))))


(def conjs1 '((¬(Traitor West)))) ; "We don't use the word 'traitor' enough." ~ Andrew Douglas Ginn

(def axioms2
  '(((=(* ?x One) ?x))
    ((=(* One ?x) ?x))
    ((=(* ?x (/ ?x)) One))
    ((=(* (/ ?x) ?x) One))
    ((=(* ?x ?w) ?v) (¬(=(* ?x ?y) ?u)) (¬(=(* ?y ?z) ?w)) (¬(=(* ?u ?z) ?v)))
    ((=(* ?u ?z) ?v) (¬(=(* ?x ?y) ?u)) (¬(=(* ?y ?z) ?w)) (¬(=(* ?x ?w) ?v)))
    ((=(* ?x ?x) One))
    ((=(* F G) H))))

(def conjs2 '((¬(= (* G F) H))))

;;; Print results
(defn- print-solution
  [solution]
  (if (seq solution)
    (doseq [step solution]
      (println step))
    (println "No solution")))

(defn deduce
  [definite-horn-clauses top-clauses]
  (let [goal?         res/res-goal?
       move-fn        (partial res/res-move definite-horn-clauses)
       heuristic-fn   res/res-heuristic]
    (print-solution (algo/A* top-clauses goal? move-fn heuristic-fn))))

(defn run-all
  []
  (do
    (println "Note: Results vectors are [(Previous utilized axiom) (Result of unification) Sum Heuristic Weight]")
    (deduce axioms1 conjs1)
    (deduce axioms2 conjs2)))

;;; Main function of executable jar
(defn -main [& args]
  "Default execution for uberjar"
  (run-all))
