(ns advent.2018
  (:require [advent.core :as a]
            [clojure.core.reducers :as r]))

;https://adventofcode.com/2018/day/1
(a/defcase day1 "2018/1.input.txt" input
  (reduce +
      (map #(Integer/parseInt %) input)))


(a/defcase day1-part2 "2018/1.input.txt" input
  (let [seq (cycle
              (map #(Integer/parseInt %) input))]
    (loop [[n & tail] seq
           tot 0
           history #{}]
      (let [res (+ n tot)]
        (if (contains? history res)
          res
          (recur tail res (conj history res)))))))



;https://adventofcode.com/2018/day/2
; utils
(defn sum-pair
  ([] [0 0])
  ([a] a)
  ([[p1 p2] [m1 m2]]
   [(+ p1 m1) (+ p2 m2)]))

(defn bool->int [b] (if b 1 0))


(a/defcase day2 "2018/2.input.txt" input
  (let [[twos threes]
        (transduce
          (comp
            (map seq)
            (map frequencies)
            (map vals)
            (map set)
            (map (juxt
                   #(bool->int (contains? % 2))
                   #(bool->int (contains? % 3)))))
          sum-pair
          input)]
    (* twos threes)))


;utils
(defn diff
  ([seq1 seq2]
   (->> (map vector seq1 seq2)
        (keep (fn [[s1 s2]] (not= s1 s2))))))


(a/defcase day2-part2 "2018/2.input.txt" input
  (loop [[word & words] (map seq input)]
    (let [diff-char (->> words
                      (map #(diff word %))
                      (filter #(= 1 (count (filter identity %))))
                      first)]
      (if (not (nil? diff-char))
        (->> (map vector word diff-char)
             (filter (comp not second))
             (map first)
             (apply str))
        (recur words)))))



;https://adventofcode.com/2018/day/3
(a/defcase day3 "2018/3.input.txt" input)
