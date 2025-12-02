(ns advent.2025.day1
  (:require [advent.core :as a]))


(def reader (a/read-input 2025 1))
(def input (reader))


(defn parse [line]
  (let [[dir n] (->> (re-matches #"([LR])(\d+)" line)
                     (drop 1))]
    [(keyword dir) (a/str->long n)]))

(defn count-zeros [[state zeros] [direction n]]
  (let [pointing (->
                  (case direction
                    :L (- state n)
                    :R (+ state n))
                  (mod 100))]
    [pointing (if (= 0 pointing) (inc zeros) zeros)]))

(defn part1 []
  (->> input
       (map parse)
       (reduce count-zeros [50 0])))

(defn count-clicks [[state clicks] [direction n]]
  (let [pointing (-> (case direction
                       :L (- state n)
                       :R (+ state n))
                     (mod 100))
        clicks (-> (case direction
                     :L (if (zero? state)
                          n
                          (+ n (- 100 state)))
                     :R (+ state n))
                   (quot 100)
                   (+ clicks))]
    [pointing clicks]))

(defn part2 []
  (->> input
       (map parse)
       (reduce count-clicks [50 0])))