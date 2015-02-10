;;;; Name: Aleksander Eskilson
;;;; KUID: 2373732
;;;; Email: aeskilson@ku.edu
;;;; Descr: Main execution function
(ns astar.core
  (:gen-class)
  (:require [astar.tile :as tile]
            [astar.algo :as algo]))

;;; Goal and Start states
(def goal
  [[1 2 3]
   [4 5 6]
   [7 8 0]])

(def s0
  [[1 2 3]
   [4 5 6]
   [0 7 8]])

(def s1
  [[6 4 2]
   [1 5 3]
   [7 0 8]])

(def s2
  [[6 4 2]
   [8 5 3]
   [1 0 7]])

(def s3
  [[8 0 7]
   [6 5 4]
   [3 2 1]])

(def s4
  [[6 4 7]
   [8 5 0]
   [3 2 1]])

(def s5
  [[1 2 3]
   [4 5 6]
   [8 7 0]])

;;; Print results
(defn- print-solution
  [solution]
  (if (seq solution)
    (doseq [step solution]
      (println step))
    (println "No solution")))

(defn run-all
  "Automatically executes and times each puzzle run with Null followed by Manhattan
  heuristic"
  []
  (let [starts      [s5]
        heuristics  [(tile/null-heuristic goal) (tile/manhattan-heuristic goal)]
        params      (for [start starts
                           heuristic heuristics]
                          [start heuristic])]
    (println "Note: All run pairs are Null followed by Manhattan")
    (doseq [param params]
      (println "====== Run ======")
      (print-solution
        (time ((partial
                (fn [[start heur]]
                  (algo/A* start (tile/goal goal) tile/move heur)))
               param))))))

(defn tile-puzzle
  "Execute a custom puzzle run"
  [start heuristic-fn]
  (let [goal?     (tile/goal goal)
        move-fn   tile/move
        heuristic (heuristic-fn goal)]
    (algo/A* start goal? move-fn heuristic-fn)))

;;; Main function of executable jar
(defn -main [& args]
  "Default execution for uberjar"
  (run-all))
