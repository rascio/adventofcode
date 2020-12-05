(ns advent.2020.day5
  (:require [advent.core :as a]
            [clojure.set :as sets]))

(def reader (a/read-input 2020 5))

(def input (reader))

(defn bsp-> [code init]
  (loop [[min max] init
         [c & tail] code]
    (if (or (= min max)
            (nil? c))
      min
      (let [distance (inc (- max min))
            half (->> (/ distance 2)
                      (int)
                      (+ min))
            range (case c
                    \F [min (dec half)]
                    \L [min (dec half)]
                    \B [half max]
                    \R [half max])]
        (recur range tail)))))

(defn seat [code]
  (let [row (subs code 0 7)
        column (subs code 7)]
    [(bsp-> row [0 127]) (bsp-> column [0 7])]))

(defn seat-id [[r c]]
  (->> (* r 8)
       (+ c)))

(defn code->seat-id [code]
  (seat-id (seat code)))

(defn part1 []
  (->> input
       (map code->seat-id)
       (apply max)))


(defn near-seats [ids id]
  (and (contains? ids (inc id))
       (contains? ids (dec id))))
(defn part2 []
  (let [seats (->> input
                   (map seat)
                   (set))
        ids (set (map seat-id seats))
        all (for [row (range 0 128)
                  col (range 0 8)]
              [row col])]
    (->> all
         (filter #(not (contains? seats %)))
         (map seat-id)
         (filter #(and (contains? ids (inc %))
                       (contains? ids (dec %)))))))