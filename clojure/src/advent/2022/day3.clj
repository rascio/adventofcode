(ns advent.2022.day3
  (:require [advent.core :as a]
            [clojure.set :as set]))


(def reader (a/read-input 2022 3))
(def input (reader))

(def priorities
  (into {}
        (concat (for [p (range 1 27)]
                  [(char (+ p 96)) p])
                (for [p (range 1 27)]
                  [(char (+ p 64)) (+ 26 p)]))))

(defn find-common-item-priority [s]
  (->> s
       (partition (/ (count s) 2))
       (map set)
       (apply set/intersection)
       (first)
       (priorities)))

(defn part-1 []
  (->> input
       (map find-common-item-priority)
       (reduce + 0)))

(defn find-badges [group]
  (->> group
       (map set)
       (reduce set/intersection)))

(defn part-2 []
  (->> input
       (partition 3)
       (mapcat find-badges)
       (map priorities)
       (reduce + 0)))