(ns learning.core
  (:require [clatrix.core :as cl])
  (:use [incanter.core :only [view sel to-matrix bind-columns]]
        [incanter.stats :only [linear-model]]
        [incanter.charts :only [xy-plot scatter-plot]]
        [incanter.datasets :only [get-dataset]]))

;;; The Iris Problem
(def iris
  (to-matrix (get-dataset :iris)))

(def X (sel iris :cols (range 1 5)))
(def Y (sel iris :cols 0))
(def iris-linear-model
  (linear-model X Y))

(defn linear-model-ols
  [MX MY]
  (let [X (bind-columns (repeat (row-count MX) 1) MX)
        Xt (cl/matrix (transpose X))
        Xt-X (cl/* Xt X)]
    (cl/* (inverse Xt-X) Xt MY)))

(def ols-linear-model
  (linear-model-ols X Y))

(def ols-linear-model-coefs
  (cl/as-vec ols-linear-model))

(defn gradient-descent
  [F' x-start alpha]
  (loop [x-old x-start]
    (let [x-new (- x-old (* alpha (F' x-old)))
          dx (- x-new x-old)]
      (if (< dx gradient-descent-precision)
        x-new
        (recur x-new)))))
