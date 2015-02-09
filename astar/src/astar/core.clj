(ns astar.core
  (:require [astar.tile-puzzle :as t]
            [astar.algo :as a]))

(def start
  [[1 2 3]
   [4 5 6]
   [7 8 0]])

(def goal
  [[6 4 2]
   [1 5 3]
   [7 0 8]])
