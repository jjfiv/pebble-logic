(ns pebble.core
  (:use seesaw.core
        seesaw.graphics
        seesaw.color)
  (:import (pebble UI PaddedLabel CommandEvaluator))
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

(defn eval-cmd [ui cmd]
  (.append (.canvasBuffer ui) (PaddedLabel. cmd)))

(defn make-ui []
  (pebble.UI. 
    (reify CommandEvaluator
      (evaluate [this ui cmd] (eval-cmd ui cmd)))))

(defn graphviz-to-bytes [graphviz-src]
  (:out (clojure.java.shell/sh "dot" "-Tpng" :in graphviz-src :out-enc :bytes)))
    
(when-not (and (resolve 'ui) (bound? (resolve 'ui)))
  (def ui (make-ui))
  (.showBytesAsImage ui (graphviz-to-bytes "digraph G { rankdir=LR; 1[label=\"s=1\"];4[label=\"t=4\"];1->2->3->4}"))
  )


(defn -main [& args]
  (println "Hello, World!"))

