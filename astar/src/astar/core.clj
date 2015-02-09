(ns astar.core
  (:require [astar.tile :as tile]
            [astar.algo :as algo]))

(def start
  [[1 2 3]
   [4 5 6]
   [0 7 8]])

(def g0
  [[1 2 3]
   [4 5 6]
   [7 8 0]])

(def g1
  [[6 4 2]
   [1 5 3]
   [7 0 8]])

(def g2
  [[6 4 2]
   [8 5 3]
   [1 0 7]])

(def g3
  [[8 0 7]
   [6 5 4]
   [3 2 1]])

(def g4
  [[6 4 7]
   [8 5 0]
   [3 2 1]])

(def g5
  [[1 2 3]
   [4 5 6]
   [8 7 0]])

(defn print-solutions
  [sols]
  (doseq [sol sols]
    (println sol)))

(defn tile-puzzle-h
  [start goal]
  (let [goal?         (tile/goal goal)
        move-fn       tile/move
        heuristic-fn  (tile/manhattan-heuristic goal)]
    (algo/A* start goal? move-fn heuristic-fn)))

(defn tile-puzzle-n
  [start goal]
  (let [goal?         (tile/goal goal)
        move-fn       tile/move
        heuristic-fn  (tile/null-heuristic goal)]
    (algo/A* start goal? move-fn heuristic-fn)))
