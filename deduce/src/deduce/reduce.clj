;;;; Name: Aleksander Eskilson
;;;; KUID: 2373732
;;;; Email: aeskilson@ku.edu
;;;; Descr: Functions specific to the Reduction and Unification
(ns deduce.reduce)

(defn bound?
  [v th]
  (if (nil? (get th v))
    false
    true))

(defn binding-value
  [v th]
  (get th v))

(defn eq?
  [v t]
  (if (= v t)
    true
    false))

(defn pair?
  [v]

(defn occur?
  [v t th]
  (cond 
    (and (var? t) (bound? t th)) (occur? v (binding-value t th) th)
    (eq? v t) true
    (var? t) false
    (not (pair? v t)) false
    :else (ormap (partial (fn [s] (occur? v s th))) (rest th))))

(defn unify 
  [p q th]
    (cond
      (= th false) false
      (and (var? p) (bound? p th)) (unify (binding-value p th) q th)
      (and (var? q) (bound? q th)) (unify p (binding-value q th) th)
      (= p q) th
      (var? p) (if (occur? p q th)
                 false
                 (conj [p q] th))
      (var? q) (if (occur? q p th)
                 false
                 (conj [q p] th))
      (or (not (pair? p)) (not (pair? q))) (if (eq? p q) 
                                             th
                                             false)
      (not= (first p) (first q)) false
      :else (map (fn [p q s] (unify p q s)) (rest q) (rest p) th)))
