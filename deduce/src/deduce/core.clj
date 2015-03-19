;;;; Name: Aleksander Eskilson
;;;; KUID: 2373732
;;;; Email: aeskilson@ku.edu
;;;; Descr: Main execution function
(ns deduce.core
  (:gen-class)
  (:require [deduce.algo :as algo]))

;;; Goal and Start states

;;; Print results
(defn- print-solution
  [solution]
  (if (seq solution)
    (doseq [step solution]
      (println step))
    (println "No solution")))

;;; Main function of executable jar
(defn -main [& args]
  "Default execution for uberjar"
  )
