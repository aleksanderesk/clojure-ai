(ns astar.core
  (:require [clojure.data.priority-map :as p]))

(defn A* [start-state goal-fn move-fn heuristic-fn])
