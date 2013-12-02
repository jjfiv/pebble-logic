(ns pebble.core
  (:gen-class))

(defn relation [id arity]
  {:id id :arity arity)

(def l1 {
          :size 1
          :relations #{ (relation "E" 2) }
         })
(def l3 {
         :size 3
         :relations #{ (relation "E" 2) }
         })

(defn -main [& args]
  (println "Hello, World!"))

