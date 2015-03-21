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
  (not (nil? (get th k))))

(defn extend-bindings
  [k v th]
  (assoc th k v))


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
  [bindings exp]
  (cond
    (= bindings false) false
    (empty? bindings) exp
    :else (for [sym exp]
            (cond
              (list? sym) (substitute-bindings bindings sym)
              (and (variable? sym) (contains? bindings sym)) (get bindings sym)
              :else sym))))


(defn instantiate
  [bindings exp]
  (substitute-bindings bindings exp))

(defn find-unique-vars
  [exp found]
  (cond
    (= exp '()) found
    :else (if (symbol? exp)
            (if (and (variable? exp) (not (contains? found exp)))
              (conj exp found)
              found)
            (find-unique-vars (rest (list (first exp))) 
                                     (find-unique-vars (rest (rest exp)) 
                                                       found)))))

(defn freevarsin
  [exp]
  (find-unique-vars exp '()))

(defn rename-variables
  [exp]
  (substitute-bindings (map (fn [y] '(y (gensym y))) (freevarsin exp)) exp))

(defn instantiate-clause
  [c a]
  (map (fn [x] (instantiate x a)) c))

(defn rename-clause
  [c]
  (map (fn [x] (rename-variables x)) c))

(defn freevarsin-clause
  [c]
  (concat (map (fn [x] (rename-variables x)) c)))

(defn resolver
  [lc kd]
  (if (negated-term? (first lc))
    (let [renamed-lc (rename-clause lc)
          unifiable (unify {} (first (rest (first renamed-lc))) (first kd))]
      (if unifiable
        [(first kd) (rename-clause (instantiate-clause (concat (rest renamed-lc) (rest kd)) unifiable))]
        false))
    false))

(defn res-move
  [clauses state]
  (for [[prev move] (filter (fn [r] (not= r false))
                     (map (fn [c] (resolver state c))
                          clauses))]
    [(into '() move) prev 1]))

(defn res-heuristic
  [state]
  (count state))

(defn res-goal?
  [state]
  (= state '()))
