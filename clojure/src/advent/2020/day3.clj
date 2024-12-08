(ns advent.2020.day3
  (:require [advent.core :as a]))

(def reader (a/read-input 2020 3))

(def input (->> (reader)
                (map vec)
                (vec)))

(defn trees [rows x y]
  (let [total (count rows)]
    (->> (iterate (fn [[x' y']]
                    [(+ x' x) (+ y' y)])
                  [0 0])
         (take-while (fn [[_ y]] (<= y total)))
         (map (fn [[x' y']]
                (let [row (get rows y')]
                  (get row (mod x' (count row))))))
         (filter #(= \# %))
         (count))))

(defn part1 []
  (trees input 3 1))

(defn part2 []
  (->> [[1 1]
        [3 1]
        [5 1]
        [7 1]
        [1 2]]
       (map (fn [[x y]] (trees input x y)))
       (reduce *)))