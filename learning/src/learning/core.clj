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

; Gradient Descent
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

;;; Problem Two: The Iris Problem, Ordinary Least Squares
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

;;; Problem Three: The Fish Problem, Bayesian Classification
;; Helpers
(defn make-sea-bass []
  #{:sea-bass
    (if (< (rand) 0.2) :fat :thin)
    (if (< (rand) 0.7) :long :short)
    (if (< (rand) 0.8) :light :dark)})

(defn make-salmon []
  #{:salmon
    (if (< (rand) 0.8) :fat :thin)
    (if (< (rand) 0.5) :long :short)
    (if (< (rand) 0.3) :light :dark)})

(defn make-sample-fish []
  (if (< (rand) 0.3) (make-sea-bass) (make-salmon)))

(def fish-training-data
  (for [i (range 10000)]
    (make-sample-fish)))

;; Probability functions
(defn probability
  [attribute & {:keys
                [category prior-positive prior-negative data]
                :or {category nil
                     data fish-training-data}}]
  (let [by-category (if category
                      (filter category data)
                      data)
        positive (count (filter attribute by-category))
        negative (- (count by-category) positive)
        total (+ positive negative)]
    (/ positive total)))

(defn evidence-of-category-with-attrs [category & attrs]
  (let [attr-probs (map #(probability % :category category) attrs)
        class-and-attr-prob (conj attr-probs
                                  (probability category))]
    (float (apply * class-and-attr-prob))))

(def prob-sum ; this should be approximately 1.0
  (+ (evidence-of-category-with-attrs :salmon) (evidence-of-category-with-attrs :sea-bass)))

(defn make-category-probability-pair
  [category attrs]
  (let [evidence-of-category (apply evidence-of-category-with-attrs category attrs)]
    {:category category
     :evidence evidence-of-category}))

(defn calculate-probability-of-category
  [sum-of-evidences pair]
  (let [probability-of-category (/ (:evidence pair)
                                   sum-of-evidences)]
    (assoc pair :probability probability-of-category)))

; Bayesian Classifier
(defn classify-by-attrs
  [categories & attrs]
  (let [pairs (map #(make-category-probability-pair % attrs)
                   categories)
        sum-of-evidences (reduce + (map :evidence pairs))
        probabilities (map #(calculate-probability-of-category sum-of-evidences %)
                           pairs)
        sorted-probabilities (sort-by :probability probabilities)
        predicted-category (last sorted-probabilities)]
    predicted-category))

; Results of classifer
(def classify-set
  [(classify-by-attrs [:salmon :sea-bass] :dark :long :fat)
   (classify-by-attrs [:salmon :sea-bass] :light :short :thin)])

(defn -main [& args]
  (println "Problem 1: Single Variable Gradient Descent on Scatter Plot")
  (println "Theoretical - Linear Model:")
  (println (str "Linear Coefficients, Y = b + mx, b=" (first (:coefs samp-linear-model)) ", m=" (second (:coefs samp-linear-model))))
  (plot-model)
  (println "Experimental - Applying Gradient Descent, Gradient Model:")
  (println (str "Gradient Coefficients, Y = b + mx, b=" (first gradient-model-coefs) ", m=" (second gradient-model-coefs)))
  (gradient-model)
  (println)
  (println "Problem 2: Multivariable Ordinary Least Squares on the Iris Data Set")
  (println "Theoretical - Linear Model:")
  (println (str "Coefficients: " iris-linear-model-coefs))
  (println "Experimental - OLS:")
  (println (str "Coefficients: " ols-linear-model-coefs))
  (println (str "Is every OLS coefficient withint acceptable bound of error? " (acceptable?
                                                                             ols-linear-model-coefs
                                                                             iris-linear-model-coefs)))
  (println)
  (println "Problem 3: Bayesian Classifier on Sea Bass and Salmon")
  (println "Training Bayesian Classifer on 10000 generated samples")
  (println (str "What is a dark, long, fat fish? (expected value, Salmon): " (first classify-set)))
  (println (str "What is a light, short, thin fish? (expected value, Sea Bass): " (second classify-set)))
  (println "END"))
