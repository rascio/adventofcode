(ns advent.2020.day10
  (:require [advent.core :as a]))

(def reader (a/read-input 2020 10))
(def input (reader) )

;Part 1
(defn group-by-diff
  [[acc groups] adapter]
  (list adapter
        (update groups (- adapter acc) inc)))
(defn multiply-adapters-diff
  [_ [_ one _ three]]
  (* one (inc three)))
(defn part1 []
  (->> input
       (map a/str->int)
       (sort)
       (reduce group-by-diff [0 [0 0 0 0]])))


;Part 2
(defn count-branches [adapters]
  (loop [acc []
         [head & tail] adapters]
    (if (nil? tail)
      acc
      (-> (take-while #(>= 3 (- % head)) tail)
          (count)
          (cons acc)
          (recur tail)))))

(defn sum-branches [adapters]
  (reduce (fn [acc adapter]
            (cons (->> (take adapter acc)
                       (reduce +))
                  acc))
          [1]
          adapters))

(defn part2 []
  (let [adapters (->> input
                      (map a/str->int)
                      (sort))
        max (last adapters)]
    (->> (concat [0] adapters [(+ 3 max)])
         (count-branches)
         (sum-branches)
         (first))))