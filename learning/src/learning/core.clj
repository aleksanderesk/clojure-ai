(ns learning.core
  (:use [clojure.core.matrix]
        [incanter.core :only [view sel to-matrix bind-columns]]
        [incanter.stats :only [linear-model]]
        [incanter.charts :only [xy-plot scatter-plot add-lines add-function]]
        [incanter.datasets :only [get-dataset]])
  (:require [clatrix.core :as cl]
            [clojure.math.numeric-tower :as nt]))

;;; Problem One: Gradient Descent
;;; Let Y = epsilon + theta1 * X
(def X1 (cl/matrix [8.401 14.475 13.396 12.127 5.044 8.339 15.692 17.108 9.253 12.029]))
(def Y1 (cl/matrix [-1.57 2.32 0.424 0.814 -2.3 0.01 1.954 2.296 -0.635 0.328]))

(def linear-samp-scatter
  (scatter-plot X1 Y1))

(defn plot-scatter []
  (view linear-samp-scatter))

;; Theoretical Result
(def samp-linear-model
  (linear-model Y1 X1))

(defn plot-model []
  (view (add-lines linear-samp-scatter
                   X1 (:fitted samp-linear-model))))

;; Experimental Result
(def precision
  0.00001)

(def m
  (row-count X1))

; Cost function for theta1
(defn th-cost
  [th eps]
  (* (/ 1 m)
     (reduce +
       (for [i (range 0 m)]
         (* (+ eps (- (* th (cl/get X1 i 0)) (cl/get Y1 i 0))) (cl/get X1 i 0))))))

; Cost function for epsilon
(defn eps-cost
  [th eps]
  (* (/ 1 m)
     (reduce +
       (for [i (range 0 m)]
         (+ eps (- (* th (cl/get X1 i 0)) (cl/get Y1 i 0)))))))

(defn gradient-descent
  [th' eps' th-start eps-start alpha]
  (loop [th-old th-start eps-old eps-start]
    (let [th-new (- th-old (* alpha (th' th-old eps-old)))
          eps-new (- eps-old (* alpha (eps' th-old eps-old)))]
      (if (and (< (nt/abs (- th-new th-old)) precision) 
               (< (nt/abs (- eps-new eps-old)) precision))
        [eps-new th-new]
        (recur th-new eps-new)))))

(def gradient-model-coefs
  (gradient-descent th-cost eps-cost 0 0 0.01))

(defn gradient-model []
  (view (add-function linear-samp-scatter
                      (fn [x] (+ (first gradient-model-coefs)
                                 (* (second gradient-model-coefs) x)))
                      5
                      17)))

;;; Problem Two: The Iris Problem
(def iris
  (to-matrix (get-dataset :iris)))

(def X2 (sel iris :cols (range 1 5)))
(def Y2 (sel iris :cols 0))

;; Theoretical result
(def iris-linear-model
  (linear-model Y2 X2))

(def iris-linear-model-coefs
  (:coefs iris-linear-model))

; Ordinary Least Squares algorithm
(defn linear-model-ols
  [MX MY]
  (let [X (bind-columns (repeat (row-count MX) 1) MX)
        Xt (cl/matrix (transpose X))
        Xt-X (cl/* Xt X)]
    (cl/* (inverse Xt-X) Xt MY)))

;; Experimental result
(def ols-linear-model
  (linear-model-ols X2 Y2))

(def ols-linear-model-coefs
  (cl/as-vec ols-linear-model))

; Is experimental result acceptable? 
(defn acceptable?
  [exper theor]
  (every? (fn [val] (< val 0.0001))
          (map -
               exper
               theor)))
