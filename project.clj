(defproject visual-de "1.0.0"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [instaparse "1.2.12"]
                 ;[org.ow2.sat4j/org.ow2.sat4j.core "2.3.4"]
                 [org.scilab.forge/jlatexmath "0.9.6"]
                 ]
  :java-source-paths ["src/main/java"]
  :source-paths ["src/main/clojure"]
  :main ^:skip-aot edu.umass.vde.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

