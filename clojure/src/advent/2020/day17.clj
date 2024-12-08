(ns advent.2020.day17
  (:require [advent.core :as a]))

(def reader (a/read-input 2020 17))
(def input (reader))

(defn parse []
  (->> input
       (mapv #(vec (.toCharArray %)))))

(defn space2d->n-dimensional-cells [dimensions layer]
  (->> (for [y (range (count layer))
             x (range (count (first layer)))
             :let [cell (get-in layer [y x])]
             :when (= \# cell)]
         (concat [x y] (repeat (- dimensions 2) 0)))
       (reduce conj #{})))

(defn directions [dimensions]
  (letfn [(dir [dimensions]
            (if (= 0 dimensions)
              [[]]
              (for [n [-1 0 1]
                    v (dir (dec dimensions))]
                (conj v n))))]
    (->> (dir dimensions)
         (filter #(not-every? zero? %)))))
(defn nearby-cells [cell]
  (mapv #(mapv + cell %) (directions (count cell))))

(defn update-cell-neighbors [cell]
  (if (some? cell)
    (update cell :neighbors inc)
    {:alive false
     :neighbors 1}))

(defn create-counters [alive]
  (let [initial-state (->> alive
                           (map #(vector % {:alive true :neighbors 0}))
                           (into {}))]
    (reduce (fn [acc cell]
              (reduce #(update %1 %2 update-cell-neighbors)
                      acc
                      (nearby-cells cell)))
            initial-state
            alive)))

(defn change-state [[coordinates {:keys [alive neighbors]}]]
  (when (or (and alive
                 (<= 2 neighbors 3))
            (and (not alive)
                 (= 3 neighbors)))
    [coordinates]))

(defn step [alive]
  (->> alive
       (create-counters)
       (mapcat change-state)
       (set)))

(defn do-steps [times cubes]
  (loop [t times
         c cubes]
    (if (= 0 t)
      c
      (recur (dec t)
             (step c)))))

(defn part1 []
  (->> (parse)
       (space2d->n-dimensional-cells 3)
       (do-steps 6)
       (count)))



(defn part2 []
  (->> (parse)
       (space2d->n-dimensional-cells 4)
       (do-steps 6)
       (count)))