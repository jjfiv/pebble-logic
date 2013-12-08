(ns pebble.core
  (:use seesaw.core
        seesaw.graphics
        seesaw.color)
  (:import (pebble UI))
  (:gen-class))

(native!)

(def graphics-config
  (.getDefaultConfiguration (.getDefaultScreenDevice (java.awt.GraphicsEnvironment/getLocalGraphicsEnvironment))))

(defn make-image [w h]
  (.createCompatibleImage graphics-config w h))

(defn create-draw-image [w h draw]
  (let [img (make-image)
        g2d (.createGraphics img)]
    (draw g2d)
    (.dispose g2d)
    img))

(defn draw-img [g img x y]
  (.drawImage g img x y nil))

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

(def grid-width 10)
(def grid-size 20)
(def grid-spacing 20)

;(defn draw-grid [g structure]
;  (let [size (:size structure)
;        rows (int (/ size grid-width))]
;    (->>
;      (range 0 size)
;      (map #(rect (* (mod % grid-width) grid-size (

(defn transform-xyt [x y theta]
  (doto (java.awt.geom.AffineTransform.)
    (.translate x y)
    (.rotate theta)))

(defn directed-arrow [g x1 y1 x2 y2]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        len (Math/sqrt (+ (* dx dx) (* dy dy)))
        delta (max 3 (* 0.05 len))]
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
    [(.getWidth bounds) (Math/abs (.getY bounds))]))

(defn midpoint [x1 y1 x2 y2]
  [(/ (+ x1 x2) 2) (/ (+ y1 y2) 2)])

(defn center-text [g string x1 y1 x2 y2]
  (let [[xw yw] (text-bb g string)
        [midx midy] (midpoint x1 y1 x2 y2)
        tx (float (- midx (/ xw 2)))
        ty (float (+ midy (/ yw 2)))]
    (.drawString g string tx ty)))

(defn draw-fill-rect [g x y w h fill-color draw-color]
  (.setColor g fill-color)
  (.fillRect g x y w h)
  (.setColor g draw-color)
  (.drawRect g x y w h))


(defn draw-node [g node]
  (let [{x :x y :y index :index} node
        [tw th] (text-bb g (str index))
        pad 8
        w (+ pad tw)
        h (+ pad th)
        x2 (+ x w)
        y2 (+ y h)]
    (draw-fill-rect g x y w h (color :lightgrey) (color :black))
    (center-text g (str index) x y x2 y2)
    (merge node {:w w :h h :x2 x2 :y2 y2})))

(defn draw-edge [g a b]
  (let [
        {ax1 :x ax2 :x2 ay1 :y ay2 :y2} a
        {bx1 :x bx2 :x2 by1 :y by2 :y2} b
        [alx aly] (midpoint ax2 ay1 ax2 ay2)
        [brx bry] (midpoint bx1 by1 bx1 by2)
        ]
    (.setColor g (color :black))
    (println [alx aly] [brx bry])
    (directed-arrow g alx aly brx bry)))


(defn paint-checkerboard [c g]
  (let [w (.getWidth c)
        h (.getHeight c)
        mw (/ w 2)
        mh (/ h 2)]
    (draw g (rect 0 0 mw mh) (style :background "#ff0000"))
    (draw g (rect mw mh w h) (style :background "#ff0000"))
    (.setColor g (color :blue))
    (center-text g "Hello World!" 0 0 mw mh)
    (directed-arrow g w 0 mw mh)
    (directed-arrow g 0 h mw mh)))

(def struc
  {:nodes 
   [
    {:x 10 :y 20 :index 0}
    {:x 60 :y 20 :index 1}
    {:x 100 :y 20 :index 2}
    {:x 150 :y 20 :index 3}
    ]
   :edges (eval-relation 2 4 (fn [[a b]] (= (inc a) b)))
   })

(defn pairs [data]
  (for [i data j data] [i j]))

(defn paint-nodes [c g]
  (let [node-list (:nodes struc)
        edges (:edges struc)]
    (println "edges" edges)
    (->> node-list 
         (map #(draw-node g %))
         (pairs)
         (filter
           (fn [[a b]] 
             (let [ai (:index a)
                   bi (:index b)]
               (contains? edges [ai bi]))))
         (map (fn [[a b]] (draw-edge g a b)))
         (doall)
         (println)
         )))
         

(defn make-canvas [paint-method]
  (doto (canvas :id :canvas :background "#ffffff" :paint paint-method)
    (request-focus!)))

(defn run 
  ([] (run paint-checkerboard))
  ([paint-method]
   (doto (frame
           :title "EF-games" :width 640 :height 480
           :content (make-canvas paint-method))
     (show!))))

(defn repl-init []
  (defn ui (pebble.UI/run)))

(defn -main [& args]
  (println "Hello, World!"))

