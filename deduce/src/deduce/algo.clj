;;;; Name: Aleksander Eskilson
;;;; KUID: 2373732
;;;; Email: aeskilson@ku.edu
;;;; Descr: A* specific functions
(ns deduce.algo
  (:require [clojure.data.priority-map :as p]))

(defrecord Node
  [action state pred g h f])

(defn pred-chain
  "Navigates through the predecessors of the solution state, builds list of
  intermediat results"
  [node]
  (loop [ls [] n node]
    (if n
      (recur (conj ls [(str "Previously resolved: " (:action n)) (str "Resolution result: " (:state n)) (:g n) (:h n) (:f n)]) (:pred n))
      ls)))

(defn A* 
  "A* as implemented with an immutable Priority Map - prioritizes by node's f value,
  handles insertion of duplicates" 
  [start goal? move heuristic]
  (loop [queue (p/priority-map-keyfn #(:f %) 
                                     start 
                                     (Node. "Start " start nil 0 (heuristic start) (heuristic start)))
         explored #{}]
    (if-let [[state node] (peek queue)]
      (if (goal? state)
        (do
          (println (str (count explored)) "nodes explored")
          (reverse (pred-chain node))) ; solution found, return node and predecessor chain
        (recur (into (pop queue)    ; explore nodes neighbors, insert with priority into map
                     (for [[s a w] (move state)
                           :when (not (contains? explored s))]
                       [s (Node. a s node (+ w (:g node)) (heuristic s) (+ w (:g node) (heuristic s)))]))
               (conj explored state))) ; add visited node to explored
      (println (str (count explored)) "terminated")))) ; print nodes explored on algorithm termination
