(ns advent.2019.day3
    (:require [advent.core :as a]))

(def reader (a/read-input 2019 3))

(def input (reader))

(defn direction [[dir v] p]
   (let [op (case dir
               :U (fn [[x y]] [x (inc y)])
               :D (fn [[x y]] [x (dec y)])
               :L (fn [[x y]] [(dec x) y])
               :R (fn [[x y]] [(inc x) y]))]
      (->> (op p)
           (iterate op)
           (take v))))
(defn line->cmds [l]
    (->> (clojure.string/split l #",")
         (map (partial re-matches #"(\w)(\d+)"))
         (map (fn [[_ dir v]] [(keyword dir) (a/str->int v)]))))
(defn cmds->matrix [cmds]
    (->> cmds
        (reduce (fn [res cmd]
                  (->> (direction cmd (last res))
                       (concat res))) 
                            
            [[0 0]])
        (drop 1)))
(defn manhattan-distance [[p1 p2] [q1 q2]]
    (+ (Math/abs (- p1 q1))
       (Math/abs (- p2 q2))))

(defn part-1 []
    (->> input
        (map line->cmds)
        (map cmds->matrix)
        (map set)
        (apply clojure.set/intersection)
        (map #(manhattan-distance [0 0] %))
        (apply min)))

(defn find-min-step [w1 w2]
    (->> w1
        (map-indexed (fn [step p]
                        (let [step2 (.indexOf w2 p)]
                            (when (pos? step2)
                                [(inc step) (inc step2)]))))
        (filter #(not (nil? %)))))

(defn part-2 []
    (->> input
        (map line->cmds)
        (map cmds->matrix)
        (apply find-min-step)
        (map (partial apply +))
        (apply min)))

