(ns advent.2022.day1
  (:require [advent.core :as a]))


(def reader (a/read-input 2022 1))
(def input (reader))

(defn split-by-elf [list]
  (reduce (fn [[head & tail] s]
            (if (not (.isBlank s))
              (-> (Integer/parseInt s)
                  (cons head)
                  (cons tail))
              (concat [[]] tail [head])))
          nil
          list))

(defn part1 []
  (->> input
       split-by-elf
       (map (partial apply +))
       (map-indexed list)
       (sort-by second)
       (last)))

(defn part2 []
  (->> input
       split-by-elf
       (map (partial apply +))
       (map-indexed list)
       (sort-by second)
       (reverse)
       (take 3)
       (map second)
       (apply +)))