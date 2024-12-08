(ns advent.2020.day2
  (:require [advent.core :as a]))

(def reader (a/read-input 2020 2))

(def input (reader))

(defn ->char [s]
  (->> s
       (.getBytes)
       (first)
       (char)))

;Part 1
(defn in-range [min max n]
  (>= max n min))

(defn part1 []
  (->> input
       (map #(rest (re-matches #"(\d+)-(\d+) (\w+): (\w+)" %)))
       (map (a/seq-map a/str->int a/str->int ->char))
       (filter (fn [[min max c psw]]
                 (->> psw
                      (filter #(= c %))
                      (count)
                      (in-range min max))))
       (count)))

; Part 2
(defn xor [b b']
  (and (or b b')
       (not (and b b'))))
(defn part2 []
  (->> input
       (map #(rest (re-matches #"(\d+)-(\d+) (\w+): (\w+)" %)))
       (map (a/seq-map a/str->int a/str->int ->char))
       (filter (fn [[idx' idx'' c psw]]
                 (let [c' (= c (get psw (dec idx')))
                       c'' (= c (get psw (dec idx'')))]
                   #_(println psw c "<=>" idx' (get psw (dec idx')) c' idx'' (get psw (dec idx'')) c'' "=>" (xor c' c''))
                   (xor c' c''))))
       (count)))