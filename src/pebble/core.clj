(ns pebble.core
  (:use seesaw.core
        seesaw.graphics
        seesaw.color)
  (:gen-class))


(def l5 
  {
   :name "l5"
   :size 5
   :relations #{ {:name "E" :arity 2 :entries {[0,1],[1,2],[2,3],[3,4]} } }
   :constants { "s" 0 "t" 4 }
   })

(defn paint-checkerboard [c g]
  (let [w (.getWidth c)
        h (.getHeight c)]
    (draw g (rect 0 0 (/ w 2) (/ h 2)) (style :background "#ff0000"))
    (draw g (rect (/ w 2) (/ h 2) w h) (style :background "#ff0000"))))


(defn run []
  (doto (frame
          :title "EF-games" :width 640 :height 480
          :content
          (canvas :id :canvas :background "#ffffff" :paint paint-checkerboard))
    (show!)))


(defn -main [& args]
  (println "Hello, World!"))

