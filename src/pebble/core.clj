(ns pebble.core
  (:use seesaw.core
        seesaw.graphics
        seesaw.color)
  (:gen-class))

(native!)

(defn tuple-from-index [arity size value]
  (->> (range 0 arity)
       (map #(mod (int (/ value (Math/pow size %))) size))
       (reverse)
       (into [])))

(defn eval-relation [arity size func]
  (->> (range 0 (int (Math/pow size arity)))
       (map #(tuple-from-index arity size %))
       (filter func)
       (into #{})))

(defn relation [name arity func size]
  {:name name
   :arity arity
   :entries (eval-relation arity size func)})

(defn line-structure [size]
  {
   :name "l5"
   :size size 
   :relations #{ (relation "E" 2 (fn [[x y]] (= (inc x) y)) size) }
   :constants { "s" 0 "t" (dec size) }
   })

(defn transform-xyt [x y theta]
  (doto (java.awt.geom.AffineTransform.)
    (.translate x y)
    (.rotate theta)))

(defn directed-arrow [g [x1 y1] [x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        len (Math/sqrt (+ (* dx dx) (* dy dy)))
        delta (max 1 (* 0.05 len))]
    (.setColor g (color :black))
    (.drawLine g x1 y1 x2 y2)
    (push g
          (.transform g (transform-xyt x1 y1 (Math/atan2 dy dx)))
          (.drawLine g (- len delta) (- delta) len 0)
          (.drawLine g (- len delta) (+ delta) len 0))
    ))

(defn text-bb [g string]
  (let [font (.getFont g)
        metrics (.getFontMetrics g font)
        bounds (.getStringBounds metrics string g)]
    [(.getWidth bounds) (.getHeight bounds)]))

(defn center-text [g string [x1 y1] [x2 y2]]
  (let [[xw yw] (text-bb g string)
        midx (/ (+ x2 x1) 2)
        midy (/ (+ y2 y1) 2)]
    (.drawString g string (float (- midx (/ xw 2))) (float (+ midy (/ yw 2))))))

(defn paint-checkerboard [c g]
  (let [w (.getWidth c)
        h (.getHeight c)
        mw (/ w 2)
        mh (/ h 2)]
    (draw g (rect 0 0 mw mh) (style :background "#ff0000"))
    (draw g (rect mw mh w h) (style :background "#ff0000"))
    (.setColor g (color :blue))
    (center-text g "Hello World!" [0 0] [mw mh])
    (directed-arrow g [w 0] [mw mh])
    (directed-arrow g [0 h] [mw mh])))

(defn run []
  (doto (frame
          :title "EF-games" :width 640 :height 480
          :content
          (canvas :id :canvas :background "#ffffff" :paint paint-checkerboard))
    (show!)))


(defn -main [& args]
  (println "Hello, World!"))

