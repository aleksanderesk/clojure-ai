(defproject astar "0.1.0-SNAPSHOT"
  :description "The classic AI algorithm A* implemented to solve the 8-Puzzle"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.priority-map "0.0.5"]
                 [org.clojure/data.int-map "0.1.0"]]
  :jvm-opts ["-Xmx3g" "-server"])
