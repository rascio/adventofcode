(ns advent.2020.day20
  (:require [advent.core :as a]))

(def reader (a/read-input 2020 20))
(def input (reader "example"))

(defn parse 
  [[state tiles id] line]
  (case state
    :new (as-> line $
           (re-matches #"Tile (\d+):" $)
           (second $)
           (a/str->int $)
           [:build (assoc tiles $ []) $])
    :build (if (empty? line)
             [:new tiles]
             (as-> line $
               (vec $)
               (update tiles id conj $)
               [:build $ id]))))
(defn get-side
  [side flipped tile]
  (as-> side $
    (case $
      :top (tile 0)
      :bottom (last tile)
      :left (map first tile)
      :right (map last tile))
    (if flipped
      (reverse $)
      $)))
(def sides [:top :bottom :left :right])
(defn add-side
  [[tile sides] id' side']
  (as-> sides $
    (assoc $ side' id')
    [tile $]))
(defn part1 []
  (->> input
       (reduce parse [:new {}])
       (second)
       (reduce
        (fn [tiles [id tile]]
          (->>
           (for [side sides
                 side' sides
                 flipped [false true]
                 flipped' [false true]
                 [id' [tile']] tiles
                 :when (= (get-side side flipped tile)
                          (get-side side' flipped' tile'))]
             [id side id' side'])
           (reduce
            (fn [acc [id side id' side']]
              (-> acc
                  (update id add-side id' side)
                  (update id' add-side id side')))
            (assoc tiles id [tile]))))
        {})
       (map (fn [[id [_ rels]]] [id rels]))
       #_#_#_(filter (fn [[_ rels]] (= 2 (count rels))))
       (map first)
       (apply *)))