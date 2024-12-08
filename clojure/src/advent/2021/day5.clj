(ns advent.2021.day5
  (:require [advent.core :as a]))

(def reader (a/read-input 2021 5))
(def input (->> (reader)
                (map #(->> (re-seq #"\d+" %)
                           (mapv a/str->int)
                           (partition 2)))))

(defn make-line
  [[[x y :as from] [x' y']]]
  (let [distance-x (Math/abs (- x x'))
        distance-y (Math/abs (- y y'))
        inc-x (/ (- x' x) (max 1 distance-x))
        inc-y (/ (- y' y) (max 1 distance-y))
        points (max distance-x distance-y)]
    #_(a/debug-forms v distance-x distance-y inc-x inc-y points)
    (->> from
         (iterate (fn [[x y]]
                    [(+ x inc-x)
                     (+ y inc-y)]))
         (take (inc points)))))
(defn part1 []
  (->> input
       (filter (fn [[[x y] [x' y']]]
                 (or (= x x')
                     (= y y'))))
       (mapcat make-line)
       (frequencies)
       (filter (fn [[_ c]] (> c 1)))
       (count)))

(defn print-map
  [points]
  (let [matrix (make-array Long/TYPE 10 10)]
    (doseq [[[x y] c] points]
      (aset matrix y x c))
    (doseq [row matrix]
      (->> row
           (map #(if (= % 0) "." (str %)))
           (reduce str)
           (println)))))
(defn part2 []
  (->> input
       (mapcat make-line)
       (frequencies)
       (filter (fn [[_ c]] (> c 1)))
       (count)
       #_(print-map)))