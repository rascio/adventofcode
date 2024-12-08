(ns advent.2021.day22
  (:require [advent.core :as a]))

(def reader (a/read-input 2021 22))

(def line-rex #"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)")
(def input (->> (reader)
                (mapv (a/re-map line-rex (cons keyword (repeat 6 a/str->int))))))

(defn in-region [[_ xs xe ys ye zs ze]]
  (and (<= -50 xs xe 50)
       (<= -50 ys ye 50)
       (<= -50 zs ze 50)))
(defn part1 []
  (->> input
       (filter in-region)
       (reduce
        (fn [acc [action xs xe ys ye zs ze]]
          (->>
           (for [x (range xs (inc xe))
                 y (range ys (inc ye))
                 z (range zs (inc ze))]
             [x y z])
           (reduce
            (fn [acc point]
              (case action
                :on (conj acc point)
                :off (disj acc point)))
            acc)))
        #{})
       (count)))