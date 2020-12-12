(ns advent.2020.day12
  (:require [advent.core :as a]))

(def reader (a/read-input 2020 12))
(def input (reader))

(def regex #"([A-Z])(\d+)")
(defn parse-instruction [instruction]
  (let [[_ command v] (re-find regex instruction)]
    [(keyword command) (a/str->int v)]))

(defn manhattan-distance [[p1 p2] [q1 q2]]
  (+ (Math/abs (- p1 q1))
     (Math/abs (- p2 q2))))

(defprotocol Navigation
  (rotate [state degrees])
  (move [state vector])
  (forward [state value]))

(defn exec [state [command v]]
  (case command
    :N (move state [0 v])
    :S (move state [0 (- v)])
    :W (move state [(- v) 0])
    :E (move state [v 0])
    :L (rotate state (- v))
    :R (rotate state v)
    :F (forward state v)))

(def directions {0 [1 0]
                 90 [0 -1]
                 180 [-1 0]
                 270 [0 1]})
(defrecord RelativePosition [position direction]
  Navigation
  (rotate [this degrees]
          (->> (mod (+ direction degrees) 360)
               (assoc this :direction)))
  (move [this vector]
        (->> (map + position vector)
             (assoc this :position)))
  (forward [this distance]
           (->> (directions direction)
                (map #(* distance %))
                (map + position)
                (assoc this :position))))

(defn part1 []
  (->> input
       (map parse-instruction)
       (reduce exec (RelativePosition. [0 0] 0))
       (:position)
       (manhattan-distance [0 0])))


;Part 2
(defn coord-rotate [vector degrees]
   (->> (int (/ (mod degrees 360) 90))
        (nth (iterate (fn [[x y]] [y (- x)])
                      vector))
        (doall)))
(defrecord WaypointPosition [position waypoint]
  Navigation
  (rotate [this degrees]
          (->> (coord-rotate waypoint degrees)
               (assoc this :waypoint)))
  (move [this vector]
        (->> (map + waypoint vector)
             (assoc this :waypoint)))
  (forward [this times]
           (->> (nth (iterate #(map + % waypoint) position)
                     times)
                (doall)
                (assoc this :position))))

(defn part2 []
  (->> input
       (map parse-instruction)
       (reduce exec (WaypointPosition. [0 0] [10 1]))
       (:position)
       (manhattan-distance [0 0])))
