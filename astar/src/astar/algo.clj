(ns astar.algo
  (:require [clojure.data.priority-map :as p]))

(defrecord Node
  [state pred g h f])

(defn pred-chain
  [node]
  (loop [ls [] n node]
    (if n
      (recur (conj ls [(:state n) (:g n) (:h n) (:f n)]) (:pred n))
      ls)))

(defn A* [start goal? move heuristic]
  (loop [queue (p/priority-map-keyfn #(:f %) 
                                     start 
                                     (Node. start nil 0 (heuristic start) (heuristic start)))
         explored #{}]
    (when-let [[state node] (peek queue)]
      (if (goal? state)
        (reverse (pred-chain node))
        (recur (into (pop queue)
                     (for [[s a w] (move state)
                           :when (not (contains? explored s))]
                       [s (Node. s node (+ w (:g node)) (heuristic s) (+ w (:g node) (heuristic s)))]))
               (conj explored state))))))
