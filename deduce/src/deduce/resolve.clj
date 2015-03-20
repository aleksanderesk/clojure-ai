;;;; Name: Aleksander Eskilson
;;;; KUID: 2373732
;;;; Email: aeskilson@ku.edu
;;;; Descr: Functions specific to the Reduction and Unification
(ns deduce.resolve)

(defn variable? 
  [x]
  (if (or (nil? x) (list? x))
    false
    (= (first (name x)) \?)))

(defn negation? 
  [x]
  (= \¬ x))

(defn negated-term? 
  [x]
  (negation? (first (name (first x)))))

(defn value 
  [x th]
  (get th x))

(defn isbound? 
  [k th]
  (not (nil? (get th k))))

(defn get-binding 
  [k th]
  '(k (get k th)))

(defn binding-var
  [bind]
  (first bind))

(defn binding-val
  [bind]
  (second bind))

(defn lookup
  [k th]
  (binding-val (get-binding k th)))

(defn extend-bindings
  [k v th]
  (assoc th k v))

(defn reuse-cons
  [x y x-y]
  (if (and (= x (first x-y)) (= y (rest x-y))) x-y '(x y)))

(defn pair?
  [x]
  (if (list? x) 
    (= (count x) 2)
    false))

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

(comment
(defn substitute-bindings
  [bindings x]
  (cond
    (= x '()) '()
    (= bindings false) false
    (empty? bindings) x
    (and (variable? x) (get-binding x bindings))
      (substitute-bindings bindings (lookup x bindings))
    (symbol? x) x
    :else (reuse-cons (substitute-bindings bindings (first x))
                      (substitute-bindings bindings (rest x))
                      x)))
)
(defn occur?
  [v t th]
  (cond
    (and (variable? t) (isbound? t th)) (occur? v (value t th) th)
    (= v t) true
    (variable? v) false
    (not (pair? t)) false
    :else (some true? (map (fn [y] (occur? v y th)) (rest t)))))

(defn merge-quit
  [m r]
  (if r
    (merge m r)
    false))

(defn unify
  [th p q]
  (cond
    (= th false) false
    (and (variable? p) (isbound? p th)) (unify th (value p th) q)
    (and (variable? q) (isbound? q th)) (unify th p (value q th))
    (= p q) th
    (variable? p) (if (occur? p q th)
                    false
                    (assoc th p q))
    (variable? q) (if (occur? q p th)
                    false
                    (assoc th q p))
    (and (symbol? p) (symbol? q) (not= p q)) false
    (not= (first p) (first q)) false
    (not= (count p) (count q)) false
    :else (reduce merge-quit {} (map (fn [pi qi] (unify th pi qi)) (rest p) (rest q)))))

(defn instantiate
  [x e]
  (substitute-bindings e x))

(defn unique-find-anywhere-if
  [exp found-so-far]
  (cond
    (= exp '()) found-so-far
    :else (if (symbol? exp)
            (if (and (variable? exp) (not (contains? found-so-far exp)))
              (conj exp found-so-far)
              found-so-far)
            (unique-find-anywhere-if (rest (list (first exp))) 
                                     (unique-find-anywhere-if (rest (rest exp)) 
                                                              found-so-far)))))

(defn freevarsin
  [x]
  (unique-find-anywhere-if x '()))

(defn rename-variables
  [x]
  (substitute-bindings (map (fn [y] '(y (gensym y))) (freevarsin x)) x))

(defn instantiate-clause
  [c a]
  (map (fn [x] (instantiate x a)) c))

(defn rename-clause
  [c]
  (map (fn [x] (rename-variables x)) c))

(defn freevarsin-clause
  [c]
  (concat (map (fn [x] (rename-variables x)) c)))

(defn unifier
  [p q]
  (instantiate p (unify {} p (rename-variables q))))

(defn resolver
  [lc kd]
  (if (negated-term? (first lc))
    (let [renamed-lc (rename-clause lc)
          unifiable (unify {} (first (rest (first renamed-lc))) (first kd))]
      (if unifiable
        (rename-clause (instantiate-clause (concat (rest renamed-lc) (rest kd)) unifiable))
        false))
    false))

(def definite-horn-clauses
  '(((Criminal ?x) (¬(American ?x)) (¬(Weapon ?y)) (¬(Sells ?x ?y ?z)) (¬(Hostile ?z)))
    ((Enemy Nono America))
    ((Owns Nono M1 ))
    ((Missile M1 ))
    ((Sells West ?x Nono) (¬(Missile ?x)) (¬(Owns Nono ?x)))
    ((American West))
    ((Weapon ?x) (¬(Missile ?x)))
    ((Hostile ?x) (¬(Hostile (Mother-of ?x) )))
    ((Hostile ?x) (¬(Enemy ?x America)))
    ((Hostile ?x) (¬(Hostile (Father-of ?x) )))))

(defn find-moves
  [state clauses]
  (filter (fn [r] (not= r false)) 
          (map (fn [c] (resolver state c)) 
               clauses)))

(defn res-move
  [s]
  (filter (fn [rhs] (let [resolved (resolve s rhs)]
                      (if resolved 
                        (list resolved (first rhs) 1)
                        false)))
          clauses))

(defn res-hueuristic
  [s]
  (count s))

(defn res-goal?
  [s]
  (= s '()))
