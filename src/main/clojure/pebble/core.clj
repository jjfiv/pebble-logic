(ns pebble.core
  (:use clojure.java.shell
        clojure.set)
  (:import (pebble UI PaddedLabel CommandEvaluator RenderMath))
  (:gen-class))

;; logical structure utils

(defn tuple-from-index [arity size value]
  (->> (range 0 arity)
       (map #(mod (int (/ value (Math/pow size %))) size))
       (reverse)
       (into [])))

(defn all-tuples [arity size]
  (->> (range 0 (int (Math/pow size arity)))
       (map #(tuple-from-index arity size %))))

(defn eval-relation [arity size func]
  (->> (all-tuples arity size)
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

(defn struc-nodes [struc]
  (if (contains? struc :nodes)
    (:nodes struc)
    (range 0 (:size struc))))

(defn relation-id [rel]
  (select-keys rel [:name :arity]))

; now we can require vocabularies to be equal for EF-game playing!
(defn vocabulary [{rels :relations consts :constants}]
  (concat
    ; relations
    (map relation-id rels)
    ; constants
    (map 
      #(-> {:name % :arity 0})
      (keys consts))))

(defn structure-contains? [{size :size} id]
  (and
    (<= 0 id)
    (< id size)))

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

;; ef gameplay

(defn pebble-id [n]
  (str "p_{" n "}"))

(defn make-ef-game [num-pebbles num-moves lstruc rstruc]
  (assert (= (vocabulary lstruc) (vocabulary rstruc)))
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
(defn -pebble-played [game]
  (let [turns (:moves game)]
    (if (= (whose-turn game) :spoiler)
      (assoc game :moves (inc turns))
      game)))
    

(defn play-pebble [game pebble which-struc which-node]
  (assert (contains? (legal-structures game) which-struc))
  (let [{struc :struc pebbles :pebbles}  (get game which-struc)
        new-map {:struc struc :pebbles (assoc pebbles pebble which-node)}]

    (assert (structure-contains? struc which-node))
    (-pebble-played
      (assoc game 
             which-struc 
             {:struc struc
              :pebbles (assoc pebbles pebble which-node)}))))

(defn play-pebbles [game pebble-num a-pebble b-pebble]
  (-> game
      (play-pebble pebble-num :A a-pebble)
      (play-pebble pebble-num :B b-pebble)))

(defn value-set [m]
  (into #{} (vals m)))

(defn active-nodes [struc pebbles]
  (merge (:constants struc) pebbles))

(defn active-relations [struc pebbles]
  (let [consts-and-pebbles (active-nodes struc pebbles)
        valid-nodes (value-set consts-and-pebbles)]
    (->> (:relations struc)
         ; drop any elements of relations we can't express using constants and these pebbles
         (map
           (fn [rel]
             (assoc rel 
                    :entries
                    (filter
                      (fn [tuple]
                        (every?  #(contains? valid-nodes %) tuple))
                      (:entries rel)))))
         ; drop any relations that have no active entries
         (filter (fn [rel] (< 0 (count (:entries rel)))))
         )))

(defn substructure [game which-struc]
  (let [{struc :struc
         pebbles :pebbles} (get game which-struc)
        nodes (active-nodes struc pebbles)
        edges (active-relations struc pebbles)]
    {:name (:name struc)
     :nodes (value-set nodes)
     :relations edges
     :constants (merge (:constants struc) pebbles)}
    ))

(defn structure [game which-struc]
  (let [{struc :struc
         pebbles :pebbles} (get game which-struc)]
    (assoc struc :constants (merge (:constants struc) pebbles))))

(defn meaningful-exprs [game which-struc]
  (let [{struc :struc pebbles :pebbles} (get game which-struc)
        consts-and-pebbles (active-nodes struc pebbles)
        id->names (clojure.set/map-invert consts-and-pebbles)]
    (->> (active-relations struc pebbles)
         ; split key, value
         (map (fn [rel]
                [(relation-id rel) (:entries rel)]))
         ; look up node names for ids
         (map (fn [[rel-id entries]]
                [rel-id 
                 (map 
                   (fn [tuple]
                     (map #(get id->names %) tuple))
                   entries)]))
         (into {}))))

(defn spoiler-wins? [game]
  ; not waiting for duplicator to play
  (when (= (whose-turn game) :spoiler)
    (not= (meaningful-exprs game :A) (meaningful-exprs game :B))))

; if the duplicator isn't done yet
(defn game-over? [game]
  (and
    (= (whose-turn game) :spoiler)
    (or
      (spoiler-wins? game)
      (= (:moves game) (:current-move game)))))

(defn winner [game]
  (if (spoiler-wins? game)
    :spoiler
    :duplicator))

(defn explain-difference [game]
  (assert (spoiler-wins? game))
  (let [a-exprs (meaningful-exprs game :A)
        b-exprs (meaningful-exprs game :B)
        a-rels (set (keys a-exprs))
        b-rels (set (keys b-exprs))]
      (->> (into #{} (concat a-rels b-rels))
           (map #(-> {:rel % :A (get a-exprs % []) :B (get b-exprs % [])}))
           ; keep only relations that are interesting
           (remove (fn [{ax :A bx :B}] (= ax bx)))
           
           ; keep only the tuples that are interesting
           (map (fn [res]
                  (let [ax (set (:A res))
                        bx (set (:B res))
                        nax (remove #(contains? bx %) ax)
                        nbx (remove #(contains? ax %) bx)]
                    (merge res {:A nax :B nbx}))))
           )))

(defn latex-and [coll]
  (clojure.string/join " \\wedge " coll))

(defn latex-rel [{rel-name :name} exprs]
  (->> exprs
       (map (fn [tuple]
              (str rel-name "("
                   (clojure.string/join "," tuple)
                   ")")))
       (latex-and)))

(defn latex-sname [id]
  (get {:A "\\mathcal{A}" :B "\\mathcal{B}"} id))

(defn difference-to-latex [coll]
  (let [phi-a (latex-and (map (fn [{rel :rel exprs :A}] (latex-rel rel exprs)) coll))
        phi-b (latex-and (map (fn [{rel :rel exprs :B}] (latex-rel rel exprs)) coll))]
    (cond
      (empty? phi-a) (str phi-b " \\models " (latex-sname :B) " and " phi-b " \\not\\models " (latex-sname :A))
      (empty? phi-b) (str phi-a " \\models " (latex-sname :A) " and " phi-a " \\not\\models " (latex-sname :B))
      :else (str phi-a " \\models " (latex-sname :A) " but " phi-b " \\models " (latex-sname :B)))))


;; game-ui

(defn player-string [player]
  (get {:spoiler "Spoiler"
        :duplicator "Duplicator"} player))

(defn game-over-message [game]
  (assert (game-over? game))
    (str "Game Over "
         (player-string (winner game))
         " wins: "
         (difference-to-latex (explain-difference game))))

(def -cur-game (atom {}))
(defn update-game [game]
  (swap! -cur-game (fn [prev-game] game)))
(defn current-game []
  (deref -cur-game))

(defn user-hint [game]
  (if (game-over? game)
    (game-over-message game)
    (let [next-player (whose-turn game)]
      (str 
        "Next player: "
        (player-string next-player)
        (if (= next-player :duplicator)
          (str " needs to play on structure: " (latex-sname (first (legal-structures game))))
          "")
        )
      )))

(defn test-game []
  (-> (make-ef-game 2 2 (line-structure 5) (line-structure 4))
      (play-pebbles "p_1" 1 1)
      (play-pebbles "p_2" 3 2)))

(defn test-game-2 []
  (-> (make-ef-game 2 2 (line-structure 5) (line-structure 4))
      (play-pebbles "p_1" 1 1)))

(defn test-game-3 []
  (-> (make-ef-game 2 2 (line-structure 5) (line-structure 4))
      (play-pebble "p_1" :A 1)))

;; graphviz
(defn graphviz-nodes [struc]
  (let [constants (:constants struc)
        id->const (clojure.set/map-invert constants)]
    (->> (struc-nodes struc)
         (map (fn [nid]
                (if (contains? id->const nid)
                  (str nid "[label=\"" (get id->const nid) "=" nid "\"]")
                  (str nid))))
         (doall))))

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

(defn has-graphviz? []
  (try
    (do
      (.startsWith "dot" (:err (clojure.java.shell/sh "dot" "-V")))
      true)
    (catch java.io.IOException ioe
      false)))

(defn graphviz-to-bytes [graphviz-src]
  (:out (clojure.java.shell/sh "dot" "-Tpng" :in graphviz-src :out-enc :bytes)))

;; UI system - local ui variable
(def ui)

(defn show-structure [struc]
  (if (has-graphviz?)
    (.showBytesAsImage ui (graphviz-to-bytes (graphviz-repr struc)))  
    (.showText ui (str struc))))

(defn show-math [latex]
  (.showImage ui (RenderMath/renderLatex latex)))

(defn show-text [text]
  (.showText ui text))

(defn clear-command []
  (-> ui (.commandField) (.setText "")))

; board-drawing
(defn show-board [ui game]
  (show-math (str "\\text{Full Structure }" (latex-sname :A)))
  (show-structure (structure game :A))
  (show-math (str "\\text{Full Structure }" (latex-sname :B)))
  (show-structure (structure game :B)))

(defn show-substructure [ui game]
  (show-math (str "\\text{Expressible Structure }" (latex-sname :A)))
  (show-structure (substructure game :A))
  (show-math (str "\\text{Expressible Structure }" (latex-sname :B)))
  (show-structure (substructure game :B)))

;; command handling
(defn game-cmd [ui cmd game]
  (cond
    (= "board" cmd) (show-board ui game)
    (= "substructure" cmd) (show-substructure ui game)
    (clojure.string/blank? cmd) (show-text (user-hint game))
    :else (show-text (str "game: " cmd)))
  (clear-command))

(defn de-cmd [ui cmd]
  (show-text cmd)
  (clear-command))

(defn eval-cmd [ui cmd]
  (if (not (empty? (current-game)))
    (game-cmd ui cmd (current-game))
    (de-cmd ui cmd)))

;; startup section

; title for frame
(def eacute "\u00e9")
(def iuml "\u00ef")
(def html-ef-games (str "Ehrenfeucht-Fra" iuml "ss" eacute " Games"))

(defn make-ui []
  (pebble.UI.
    html-ef-games
    (reify CommandEvaluator
      (evaluate [this ui cmd] (eval-cmd ui cmd)))))

(defn init []
  (def ui (make-ui))
  (show-math "\\forall x,y: E(x,y)")
  (show-structure (line-structure 4)))
    
;; load in repl
(when-not (and (resolve 'ui) (bound? (resolve 'ui)))
  (init))


