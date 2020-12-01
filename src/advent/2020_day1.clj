(ns advent.2020-day1
  (:require [advent.core :as a]))

(def reader (a/read-input 2020 1))

(def input (->> (reader)
                (map #(Integer/parseInt %))))

(defn part1 []
  (println input)
  (->> (for [n input n' input
             :when (and (not= n n')
                        (= 2020 (+ n n')))]
         (* n n'))
       (first)))

(defn part2 []
  (println input)
  (->> (for [n input n' input n'' input
             :when (and (not= n n' n'')
                        (= 2020 (+ n n' n'')))]
         (* n n' n''))
       (first)))