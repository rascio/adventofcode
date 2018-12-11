(ns advent.2018
  (:require [advent.core :as a]
            [clojure.java.io :as io]))

(a/defcase day1 "2018/1.input.txt"
  (reduce +
      (map #(Integer/parseInt %) 
        (line-seq (io/reader *in*)))))
