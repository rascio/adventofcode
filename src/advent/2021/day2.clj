(ns advent.2021.day2
  (:require [advent.core :as a]))

(def reader (a/read-input 2021 2))
(def input (reader))

(defn part1 []
  (->> input
       (map (a/re-map #"(\w+) (\d+)" [keyword a/str->int]))
       (reduce 
        (fn [[h d] [command X]]
          (case command
            :forward [(+ h X) d]
            :down [h (+ d X)]
            :up [h (- d X)]))
        [0 0])
       (apply *)))

(defn part2 []
  (->> input
       (map (a/re-map #"(\w+) (\d+)" [keyword a/str->int]))
       (reduce
        (fn [[h d aim] [command X]]
          (case command
            :forward [(+ h X) (+ d (* aim X)) aim]
            :down [h d (+ aim X)]
            :up [h d (- aim X)]))
        [0 0 0])
       (take 2)
       (apply *)))