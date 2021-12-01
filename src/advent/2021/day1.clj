(ns advent.2021.day1
  (:require [advent.core :as a]))

(def reader (a/read-input 2021 1))
(def input (reader))

(defn part1 []
  (let [[head & tail] (map #(Integer/parseInt %) input)]
    (->> tail
         (reduce
          (fn [[prev count] v]
            (if (> v prev)
              [v (inc count)]
              [v count]))
          [head 0])
         (second))))

(defn part2 []
  (let [[a b c & tail] (map #(Integer/parseInt %) input)]
    (->> tail
         (reduce
          (fn [[[a b c] count] v]
            (let [prev (+ a b c)
                  current (+ v a b)
                  c (if (> current prev) (inc count) count)]
              [[v a b] c]))
          [[c b a] 0])
         (second))))