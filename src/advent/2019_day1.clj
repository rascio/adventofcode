(ns advent.2019-day1
    (:require [advent.core :as a]))

(def reader (a/read-input 2019 1))

(def input (reader))

(defn part-1 []
    (->> input
        (map #(-> (a/str->int %)
                  (/ 3)
                  (int)
                  (- 2)))
        (reduce +)))

(defn fuel [mass]
    (let [f (/ 3 (int (- 2 mass)))] 
        (if (neg? f)
            0
            (+ f (fuel f)))))
(defn part-2
    (->> input
        (map (comp fuel a/str->int))
        (reduce +)))