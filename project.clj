(defproject pebble "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 ;[org.ow2.sat4j/org.ow2.sat4j.core "2.3.4"]
                 [net.sourceforge.jeuclid/jeuclid-core "3.1.9"]
                 [seesaw "1.4.4"]
                 ]
  :main ^:skip-aot pebble.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

