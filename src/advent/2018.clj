(ns advent.2018
  (:require [advent.core :as a]
            [clojure.java.io :as io]))

;https://adventofcode.com/2018/day/1
(a/defcase day1 "2018/1.input.txt"
  (reduce +
      (map #(Integer/parseInt %)
        (line-seq (io/reader *in*)))))

(a/defcase day1-part2 "2018/1.input.txt"
  (let [seq (cycle
              (map
                #(Integer/parseInt %)
                (line-seq (io/reader *in*))))]
    (loop [[n & tail] seq
           tot 0
           history #{}]
      (let [res (+ n tot)]
        (if (contains? history res)
          res
          (recur tail res (conj history res)))))))
