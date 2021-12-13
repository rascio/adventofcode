(ns advent.2021.day10
  (:require [advent.core :as a]))

(def reader (a/read-input 2021 11))
(def input (->> (reader)
                (mapv (partial mapv a/str->int))))

(def W (count (first input)))
(def H (count input))

(def directions
  (for [x [-1 0 1]
        y [-1 0 1]
        :when (not (= 0 x y))]
    [y x]))

(defn adjacents
  [point]
  (->> directions
       (map #(mapv + point %))
       (filter (fn [[y x]] (and (< -1 x W)
                                (< -1 y H))))))
(defn increment-levels
  [levels]
  (->>
   (for [x (range 0 W)
         y (range 0 H)]
     [y x])
   (reduce
    (fn [[acc flashes] point]
      (let [level (inc (get-in acc point))]
        [(assoc-in acc point level)
         (if (> level 9)
           (conj flashes point)
           flashes)]))
    [levels []])))

(defn flash
  ([[levels checks]] (flash 0 levels checks))
  ([flashes levels [check & remaining]]
   (if (nil? check) 
     [levels flashes]
     (let [level (get-in levels check)]
       (cond
         (= level 0) (recur flashes
                            levels
                            remaining)
         (>= level 9) (recur (inc flashes)
                             (assoc-in levels check 0)
                             (concat (adjacents check)
                                     remaining))
         :else (recur flashes
                      (update-in levels check inc)
                      remaining))))))

(defn step [levels] (->> levels increment-levels flash))

(defn part1 [n]
  (loop [steps   n
         levels  input
         flashes 0]
    (if (zero? steps)
      flashes
      (let [[new-levels step-count] (step levels)]
        (recur (dec steps) new-levels (+ flashes step-count))))))

(defn part2 []
  (loop [steps  0
         levels input]
    (let [[new-levels flash-count] (step levels)]
      (if (= flash-count (* W H))
        (inc steps)
        (recur (inc steps) new-levels)))))
