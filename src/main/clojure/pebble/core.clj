(ns pebble.core
  (:use clojure.java.shell
        clojure.set)
  (:import (pebble UI PaddedLabel CommandEvaluator))
  (:gen-class))

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
   :name (str "l" size)
   :size size 
   :relations #{ (relation "E" 2 (fn [[x y]] (= (inc x) y)) size) }
   :constants { "s" 0 "t" (dec size) }
   })

(defn structure-contains? [{size :size} id]
  (println "size=" size)
  (and
    (<= 0 id)
    (< id size)))

(defn graphviz-nodes [{size :size constants :constants}]
  (let [id->const (clojure.set/map-invert constants)]
    (->> (range 0 size)
         (map (fn [nid]
                (if (contains? id->const nid)
                  (str nid "[label=\"" (get id->const nid) "=" nid "\"]")
                  (str nid))))
         (doall))))

(defn find-first [f coll]
  (first (filter f coll)))

(defn find-edge-relation [{rels :relations}]
  (->> rels
       (filter #(= (:arity %) 2))
       ; select "E" predicate if available else any 2-relation
       ((fn [rels-2]
         (if (empty? rels-2)
           nil
           (if-let [e-rel (find-first #(= (:name %) "E") rels-2)]
             e-rel
             (first rels-2)))))))

(defn graphviz-edges [struc]
  (if-let [edges (:entries (find-edge-relation struc))]
    (map
      (fn [[x y]]
        (str x "->" y)) edges)
    ""))

(def graphviz-rankdir "rankdir=LR")
(def graphviz-opts (str graphviz-rankdir ";" "ratio=\"compress\";"))
(defn graphviz-repr [struc]
  (->>
    (concat (graphviz-nodes struc) (graphviz-edges struc))
    (clojure.string/join ";")
    ((fn [guts]
       (str "digraph " (:name struc) " {" graphviz-opts guts "}")))))


(defn make-ef-game [num-pebbles num-moves lstruc rstruc]
  (let [k (max 0 num-pebbles)
        n (max k num-moves)]
    {
     :pebbles k
     :moves n

     :current-move 0

     ; the two structures, and pebbles on top
     :A {:struc lstruc :pebbles {}}
     :B {:struc rstruc :pebbles {}}
     }))

(defn pebbles [game which-struc]
  (:pebbles (get game which-struc)))

(defn pebble-id [n]
  (str "x_" n))

; spoiler plays first
(defn whose-turn [game]
  (let [npa (count (pebbles game :A))
        npb (count (pebbles game :B))]
    (if (= npa npb)
      :spoiler
      :duplicator)))

; which-structure next
(defn legal-structures [game]
  (let [npa (count (pebbles game :A))
        npb (count (pebbles game :B))]
    (cond
      (= npa npb) #{:A :B}
      (< npa npb) #{:A}
      :else #{:B})))

; increase move count if duplicator just played
(defn pebble-played [game]
  (let [turns (:moves game)]
    (if (= (whose-turn game) :spoiler)
      (assoc game :moves (inc turns))
      game)))
    

(defn play-pebble [game pebble-num which-struc which-node]
  (assert (contains? (legal-structures game) which-struc))
  (let [pid (pebble-id pebble-num)
        {struc :struc pebbles :pebbles}  (get game which-struc)
        new-map {:struc struc :pebbles (assoc pebbles pid which-node)}]
    (println new-map)
    (assert (structure-contains? struc which-node))
    (pebble-played
      (assoc game 
             which-struc 
             {:struc struc
              :pebbles (assoc pebbles pid which-node)}))))

    
        


; if the duplicator isn't done yet
(defn game-over? [game]
  (and
    (= (whose-turn game) :spoiler)
    (= (:moves game) (:current-move game))))



(defn eval-cmd [ui cmd]
  (.append (.canvasBuffer ui) (PaddedLabel. cmd)))

(defn make-ui []
  (pebble.UI. 
    (reify CommandEvaluator
      (evaluate [this ui cmd] (eval-cmd ui cmd)))))

(defn has-graphviz? []
  (try
    (do
      (.startsWith "dot" (:err (clojure.java.shell/sh "dot" "-V")))
      true)
    (catch java.io.IOException ioe
      false)))

(defn graphviz-to-bytes [graphviz-src]
  (:out (clojure.java.shell/sh "dot" "-Tpng" :in graphviz-src :out-enc :bytes)))
    
(when-not (and (resolve 'ui) (bound? (resolve 'ui)))
  (def ui (make-ui))
  (.showBytesAsImage ui (graphviz-to-bytes "digraph G { rankdir=LR; 1[label=\"s=1\"];4[label=\"t=4\"];1->2->3->4}"))
  )


(defn -main [& args]
  (println "Hello, World!"))

