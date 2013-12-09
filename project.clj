(defproject pebble "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/math.combinatorics "0.0.7"]
                 ;[org.ow2.sat4j/org.ow2.sat4j.core "2.3.4"]
                 [org.scilab.forge/jlatexmath "0.9.6"]
                 ]
  :java-source-paths ["src/main/java"]
  :source-paths ["src/main/clojure"]
  :main ^:skip-aot pebble.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

