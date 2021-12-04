(ns advent.2018.day5
  (:require [advent.core :as a]))

(def reader (a/read-input 2018 5))
(if true
  (def input (first (reader)))
  (def input "dabAcCaCBAcCcaDA"))

(defn interact
  [p p']
  (and (not= p p')
       (= (Character/toLowerCase p) (Character/toLowerCase p'))))
(defn remove-interaction
  [polymers idx]
  (str (.substring polymers 0 idx)
       (.substring polymers (+ 2 idx))))
(defn step
  [p idx]
  (when (interact (nth p idx)
                (nth p (inc idx)))
    (remove-interaction p idx)))
(defn process
  ([polymers] (process polymers 0))
  ([polymers idx]
   (if (>= idx (dec (count polymers)))
     polymers
     (let [next (step polymers idx)]
       (if (nil? next)
         (recur polymers (inc idx))
         (recur next (max 0 (dec idx))))))))
(defn part1 []
  (->> input
       (process)
       (count)))

(defn part2 []
  (->> input
       (reduce #(conj %1 (Character/toLowerCase %2)) #{})
       (map
        (fn [test]
          (->> input
               (filterv #(not
                          (or (= test %)
                              (= test (Character/toLowerCase %)))))
               (apply str)
               (process)
               (count))))
       (filter #(> % 0))
       (sort)))