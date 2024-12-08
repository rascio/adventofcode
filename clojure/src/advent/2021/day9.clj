(ns advent.2021.day9
  (:require [advent.core :as a]))

(def reader (a/read-input 2021 9))
(def input (->> (reader)
                (mapv #(mapv a/str->int %))))
(def w (count (first input)))
(def h (count input))

(def directions
  [[-1 0]
   [0 -1]
   [0 1]
   [1 0]])

(defn adjacents [y x]
  (for [[inc-x inc-y] directions
        :let [x' (+ x inc-x)
              y' (+ y inc-y)]
        :when (and (<= 0 x')
                   (<= 0 y')
                   (> w x')
                   (> h y'))]
    [y' x']))

(defn low-points []
  (for [x (range 0 w)
        y (range 0 h)
        :let [v (get-in input [y x])
              near (adjacents y x)]
        :when (->> near
                   (map #(get-in input %))
                   (filter #(>= v %))
                   (count)
                   (= 0))]
    [y x]))
(defn part1 []
  (->>
   (low-points)
   (map #(get-in input %))
   (map inc)
   (reduce +)))

(defn get-basin
  ([point] (get-basin #{} point))
  ([basin [y x :as point]]
   (->> (adjacents y x)
        (filter (complement basin))
        (filter #(not= 9 (get-in input %)))
        (reduce get-basin 
                (conj basin point)))))
(defn part2 []
  (->>
   (low-points)
   (map get-basin)
   (map count)
   (sort #(compare %2 %1))
   (take 3)
   (reduce *)))