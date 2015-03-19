(defproject deduce "0.1.0-SNAPSHOT"
  :description "A Backwards Chaining Automated Deduction algorithm for the Horn
               Clause subset of First Order Logic"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.priority-map "0.0.5"]
                 [org.clojure/data.int-map "0.1.0"]]
  :source-paths ["src"]
  :main deduce.core)
