;;;; Name: Aleksander Eskilson
;;;; KUID: 2373732
;;;; Email: aeskilson@ku.edu
;;;; Descr: Functions specific to the Reduction and Unification
(ns deduce.resolve)

(defn variable? 
  [x]
  (if (or (nil? x) (seq? x))
    false
    (= (first (name x)) \?)))

(defn negation? 
  [x]
  (= \¬ x))

(defn negated-term? 
  [x]
  (if (symbol? (first x))
    (negation? (first (name (first x))))
    false))

(defn isbound? 
  [k th]
  (contains? th k))

(defn occur?
  [v t th]
  (cond
    (and (variable? t) (isbound? t th)) (occur? v (get th t) th)
    (= v t) true
    (variable? v) false
    :else (some true? (map (fn [y] (occur? v y th)) 
                           (rest t)))))

(defn merge-quit
  [m r]
  (if (and m r)
    (merge m r)
    false))

(defn unify
  [th p q]
  (cond
    (= th false) false
    (and (variable? p) (isbound? p th)) (unify th (get th p) q)
    (and (variable? q) (isbound? q th)) (unify th p (get th q))
    (= p q) th
    (variable? p) (if (occur? p q th)
                    false
                    (assoc th p q))
    (variable? q) (if (occur? q p th)
                    false
                    (assoc th q p))
    (and (symbol? p) (symbol? q) (not= p q)) false
    (and (symbol? p) (not (symbol? q))) false
    (and (not (symbol? p)) (symbol? q)) false
    (not= (first p) (first q)) false
    (not= (count p) (count q)) false
    :else (reduce merge-quit {} (map (fn [pi qi] (unify th pi qi)) (rest p) (rest q)))))

(defn substitute-bindings
  [bindings expression]
  (cond (list? expression) (map #(substitute-bindings bindings %) expression)
        (and (variable? expression) (contains? bindings expression)) (get bindings expression)
        :else expression))


(defn free-vars-in-clause
  [clause]
  (cond (list? clause) (flatten (map free-vars-in-clause clause))
        (variable? clause) clause
        :else nil))

(defn rename-clause
  [clause]
  (let [bindings-list (filter #(not (nil? %)) (free-vars-in-clause clause))
        bindings-set  (reduce conj #{} bindings-list)
        bindings-map  (reduce merge {} (map (fn [y] {y (gensym y)}) bindings-set))]
    (substitute-bindings bindings-map
                         clause)))

(defn resolver
  [lc kd]
  (if (negated-term? (first lc))
    (let [renamed-lc (rename-clause lc)
          unifiable (unify {} (first (rest (first renamed-lc))) (first kd))]
      (if unifiable
        [(first kd) (substitute-bindings unifiable (concat (rest renamed-lc) (rest kd)))]
        false))
    false))

(defn res-move
  [clauses state]
  (for [[prev move] (filter (fn [r] (not= r false))
                     (map (fn [c] (resolver state c))
                          clauses))]
    [move prev 1]))

(defn res-heuristic
  [state]
  (count state))

(defn res-goal?
  [state]
  (= state '()))
