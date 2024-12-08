(ns advent.2020.day6
  (:require [advent.core :as a]
            [clojure.set :as s]))

(def reader (a/read-input 2020 6))
(def input (reader))

(defn split-in-groups [[current & others] line]
  (if (empty? line)
    (concat [[] current] others)
    (cons (->> (.toCharArray line)
               (set)
               (conj current))
          others)))

(defn part1 []
  (->> (reduce split-in-groups [] input)
       (map #(apply s/union %))
       (map count)
       (reduce +)))


(defn part2 []
  (->> (reduce split-in-groups [] input)
       (map #(apply s/intersection %))
       (map count)
       (reduce +)))
;3437