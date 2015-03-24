;;;; Name: Aleksander Eskilson
;;;; KUID: 2373732
;;;; Email: aeskilson@ku.edu
;;;; Descr: Functions specific to the Resolution and Unification
(ns deduce.resolve
  (:require [deduce.utils :refer [merge-quit]]))

(defn variable?
  "Determines if the expression is a variable symbol"
  [x]
  (if (or (nil? x) (seq? x))
    false
    (= (first (name x)) \?)))

(defn negation?
  [x]
  (= \¬ x))

(defn negated-term?
  "Determines if the expression is a negated term of a clause"
  [x]
  (if (symbol? (first x))
    (negation? (first (name (first x))))
    false))

(defn isbound?
  "Determines if a variable in a clause is bound to a symbol"
  [k th]
  (contains? th k))

(defn occur?
  "Determines if a variable occurs in two clauses with conflicting bindings"
  [v t th]
  (cond
    (and (variable? t) (isbound? t th)) (occur? v (get th t) th)
    (= v t) true
    (variable? v) false
    :else (some true? (map #(occur? v % th) 
                           (rest t)))))

(defn unify
  "Given a map of bindings, a clause p and a clause q, attempts to determine
  if those clauses can unify. If so, returns a dicionary representing the
  bindings of variables mapped to replacement-symbols"
  [th p q]
  (cond
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
  "Given a map of variable -> symbol or variable -> replacement-name bindings,
  recursively checks each symbol in a clause and replaces it with its binding,
  else returns the symbol. Returns the original clause with its bindings replaced"
  [bindings expression]
  (cond (list? expression) (map #(substitute-bindings bindings %) expression)
        (and (variable? expression) (contains? bindings expression)) (get bindings expression)
        :else expression))

(defn free-vars-in-clause
  "Inspects each symbol in a clause to generate a list of variables. List may
  contain duplicate variable names and nil values"
  [clause]
  (cond (list? clause) (flatten (map free-vars-in-clause clause))
        (variable? clause) clause
        :else nil))

(defn rename-clause
  "Identifies the set of unique variables for a clause of expressions. Generates
  unique names for each variable in the set and replaces the original variables
  returning the original clause with only variables renamed"
  [clause]
  (let [bindings-list (filter #(not (nil? %)) (free-vars-in-clause clause))
        bindings-set  (reduce conj #{} bindings-list)
        bindings-map  (reduce merge {} (map #({% (gensym %)}) bindings-set))]
    (substitute-bindings bindings-map
                         clause)))

(defn resolver
  "Attempts to resolve two clauses. If clauses can be unified, substitutes
  the bindings of the newly generated unified clause with the unifiable bindings,
  returning a resolved clause"
  [lc kd]
  (if (negated-term? (first lc))
    (let [renamed-lc (rename-clause lc)
          unifiable-vars (unify {} (first (rest (first renamed-lc))) (first kd))]
      (if unifiable-vars
        [kd
         (substitute-bindings unifiable-vars (apply list (concat (rest renamed-lc) (rest kd))))]
        false))
    false))

(defn res-move
  "Produces a vector of state, action, weight triples for a successful
  unification move by attempting to resolve the current state against all
  axioms"
  [clauses state]
  (for [[prev move] (filter #(not= % false)
                            (map #(resolver state %)
                                 clauses))]
    [move prev 1]))

(defn res-heuristic
  [state]
  (count state))

(defn res-goal?
  [state]
  (= state '()))
