(ns astar.tile
  (:require [clojure.data.int-map :as i]))

(defn- swap
[coll i j]
(assoc coll j (coll i) i (coll j)))

(defn- matrix->coords
"Returns a map providing the coordinates of each element"
[matrix]
(into (i/int-map) 
      (for [[x row] (map-indexed vector matrix)
            [y val] (map-indexed vector row)]
        [val [x y]])))

(defn- coords->matrix
"Returns a nested vector representing the matrix of a map of coordinates to values"
[coords]
(reduce (fn [coll [value coord]] 
          (assoc-in coll coord value))
        [[0 0 0][0 0 0][0 0 0]] ; skeleton collection to build upon
        coords))

(defn- manh-dist
"Computes the Manhattan distance between any two points"
[u v]
(reduce +
        (map (fn [[a b]] (Math/abs (- a b)))
             (map vector u v))))

;;; goal-fn
(defn goal
"Returns a goal function accepting a state to determine if the goal was reached"
[goal]
(fn [state] (= state goal)))

;;; move-fn
(defn- find-neighbors
"Returns a list of tile values that are neighbors to the blank"
[state-coords]
(for [[value coord] state-coords
      :when (= 1 (manh-dist coord (get state-coords 0)))]
  value))

(defn- find-moves
"Returns a list of possible move states"
[state]
(let [state-coords (matrix->coords state)]  
  (for [value (find-neighbors state-coords)]
    (->> value
         (swap state-coords 0)
         coords->matrix))))

(defn move
"Returns a list of state-action-weight moves obtainable from the provided state"
[state]
(for [move (find-moves state)]
  [move "?" 1]))

;;; heuristic-fn
(defn null-heuristic
"Returns a function returning 0 as the remaining distance estimate"
[state]
(fn [_] 0))

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

(defn manh-heur 
  [[state goal]] 
  (let [goal-coords (matrix->coords goal) 
        state-coords (matrix->coords state)] 
    (reduce + 
            (for [[value s-coord] state-coords 
                  :let [g-coord (get goal-coords value)] 
                  :when (not= 0 value)] 
              (manh-dist s-coord g-coord)))))

