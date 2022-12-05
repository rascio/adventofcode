(ns advent.2022.day4
  (:require [advent.core :as a]))

(def reader (a/read-input 2022 4))
(def input (reader ))

(defn parse-assignment [line]
  (->> line
       (a/re-map #"(\d+)-(\d+),(\d+)-(\d+)" 
                 (repeat 4 a/str->int))
       (partition 2)
       (map vec)))

(defn complete-overlap? [[[s e] [s' e']]]
  (or (and (>= s s')
           (<= e e'))
      (and (<= s s')
           (>= e e'))))

(defn part-1 []
  (->> input
       (map parse-assignment)
       (filter complete-overlap?)
       (count)))

(defn get-overlap [assignments] 
  (let [tmp (sort assignments)
        [_ e-min] (first tmp)
        [s-max _] (last tmp)]
    (>= e-min s-max)))

(defn part-2 []
  (->> input
       (map parse-assignment)
       (filter get-overlap)
       (count)))