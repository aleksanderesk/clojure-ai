(ns astar.core
  (:require [clojure.data.priority-map :as p]
            [clojure.data.int-map :as i]))

(def start
  [[1 2 3]
   [4 5 6]
   [7 8 0]])

(def goal
  [[6 4 2]
   [1 5 3]
   [7 0 8]])

(defn matrix->coords
  "Returns a map providing the coordinates of each element"
  [matrix]
  (into (i/int-map) 
        (for [[x row] (map-indexed vector matrix)
              [y val] (map-indexed vector row)]
          [val [x y]])))

;;; goal-fn
(defn puzzle-goal
  "Returns a goal function accepting a state to determine if the goal was reached"
  [goal]
  (fn [state]
    (= state goal)))

;;; move-fn
(defn puzzle-move
  "Returns a list of states possible to move to"
  [state])

;;; heuristic-fn
(defn null-heuristic
  "Returns a function returning 0 as the remaining distance estimate"
  [_]
  0)

(defn manh-dist
  "Computes the Manhattan distance between any two points"
  [u v]
  (reduce +
          (map (fn [[a b]] (Math/abs (- a b)))
               (map vector u v))))
      
(defn manhattan-heuristic
  "Returns the a function to compute the sum of the Manhattan distance between
   all points for given goal and state matrices"
  [goal]
  (fn [state]
    (let [goal-coords   (matrix->coords goal)
          state-coords  (matrix->coords state)]
      (reduce +
              (for [[value s-coord] state-coords
                    :let [g-coord (get goal-coords value)]
                    :when (not= 0 value)] ; don't include blank in Manhattan calculation
                (manh-dist s-coord g-coord))))))

(defn A* [state-state goal-fn move-fn heuristic-fn]
  )
