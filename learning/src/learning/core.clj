(ns learning.core
  (:require [clatrix.core :as cl])
  (:use [incanter.charts :only [xy-plot scatter-plot]]
        [incanter.core :only [view sel to-matrix]]
        [incanter.datasets :only [get-dataset]]))

;;; The Iris Problem


(defn gradient-descent
  [F' x-start alpha]
  (loop [x-old x-start]
    (let [x-new (- x-old (* alpha (F' x-old)))
          dx (- x-new x-old)]
      (if (< dx gradient-descent-precision)
        x-new
        (recur x-new)))))
