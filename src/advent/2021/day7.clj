(ns advent.2021.day7
  (:require [advent.core :as a]))

(def reader (a/read-input 2021 7))
(def input (->> (reader)
                (mapcat #(re-seq #"\d+" %))
                (map a/str->int)))
(def max-pos (apply max input))
(defn part1 []
  (->> (range 0 (count input))
       (reduce
        (fn [min idx]
          (loop [[crab & others] input
                 cost 0]
            (cond 
              (>= cost min) min
              (nil? crab) cost
              :else (->> (Math/abs (- crab idx))
                         (+ cost)
                         (recur others)))))
        Integer/MAX_VALUE)))

(defn part2 []
  (->> (range 0 (count input))
       (reduce
        (fn [min idx]
          (loop [[crab & others] input
                 cost 0]
            (cond
              (>= cost min) min
              (nil? crab) cost
              :else (as-> (Math/abs (- crab idx)) $
                      (* (inc $) (/ $ 2))
                      (recur others
                             (+ cost $))))))
        Integer/MAX_VALUE)))