(ns advent.2018
  (:require [advent.core :as a]
            [clojure.java.io :as io]))

;https://adventofcode.com/2018/day/1
(a/defcase day1 "2018/1.input.txt"
  (reduce +
      (map #(Integer/parseInt %)
        (line-seq (io/reader *in*)))))

(a/defcase day1-part2 "2018/1.input.txt"
  (TODO))
