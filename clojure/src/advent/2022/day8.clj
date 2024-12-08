(ns advent.2022.day8
  (:require [advent.core :as a]
            [clojure.set :as set]))

(def reader (a/read-input 2022 8))
(def input (reader))

(defn parse-tree-heights [input]
  (->> input
       (map (fn [line]
              (->> line
                   (re-seq #"\d")
                   (map a/str->int))))
       (to-array-2d)))

(defn visible-trees [trees]
  (let [h (count trees)
        w (count (first trees))
        process (fn [rows cols]
                  (->> (map list rows cols)
                       (reduce (fn [[prev acc] [y x]]
                                 (let [height (aget trees y x)]
                                   (if (< prev height)
                                     [height (conj acc [[x y] height])]
                                     [prev acc])))
                               [-1 #{}])
                       (second)))]
    (apply set/union 
           (concat (for [y (range h)]
                     (set/union
                      (process (repeat w y)
                               (range w))
                      (process (repeat w y)
                               (reverse (range w)))))
                   (for [x (range w)]
                     (set/union
                      (process (range h)
                               (repeat h x))
                      (process (reverse (range h))
                               (repeat h x))))))))

(defn part-1 []
  (->> input
       (parse-tree-heights)
       (visible-trees)))

(defn scenic-score
  [col row trees]
  (let [h (count trees)
        w (count (first trees))
        height (aget trees row col)
        count-trees (fn [dx dy]
                      (->> (map list dx dy)
                           (reduce (fn [acc [x y]]
                                     (let [h (aget trees y x)]
                                       (if (>= h height) 
                                         (reduced (inc acc))
                                         (inc acc))))
                                   0)))
        top-count (count-trees (repeat w col)
                               (reverse (range row)))
        right-count (count-trees (range (inc col) w)
                                 (repeat h row))
        bottom-count (count-trees (repeat w col)
                                  (range (inc row) h))
        left-count (count-trees (reverse (range col))
                                (repeat h row))]
    (* top-count right-count bottom-count left-count)))

(defn best-scenic-score
  [trees]
  (->>
   (for [y (range (count trees))
         x (range (count (first trees)))]
     (scenic-score x y trees))
   (sort >)
   (first)))

(defn part-2 []
  (->> input
       (parse-tree-heights)
       (best-scenic-score)))