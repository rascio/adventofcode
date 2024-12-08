(ns advent.2023.day1
  (:require [advent.core :as a]
            [clojure.string :as str]))


(def reader (a/read-input 2023 1))
(def input (reader "example"))

(def number-values ["one" "two" "three" "four" "five" "six" "seven" "eight" "nine"])

(defn get-numbers [s]
  (let [numbers (re-seq #"\d" s)]
    (->> (str (first numbers) (last numbers))
         (Long/parseLong))))

(defn part1 []
  (->> input
       (map get-numbers)
       (reduce +)))

(defn replace-word-with-number [s]
  (let [word-to-replace (->> number-values
                             (map (fn [v] [(.indexOf s v) v]))
                             (filter (fn [[k _]] (<= 0 k)))
                             (sort-by first)
                             (map second)
                             (first))]
    (if (nil? word-to-replace)
      s
      (->> (.indexOf number-values word-to-replace)
           (inc)
           (str)
           (.replaceFirst s word-to-replace)
           (recur)))))
(defn get-number-words [s]
  (get-numbers (replace-word-with-number s)))
(defn part2 []
  (->> input
       (map replace-word-with-number)
       (map get-numbers)
       ))