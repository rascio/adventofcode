(ns advent.2021.day21
  (:require [advent.core :as a]))

(def players (if true [5 10] [4 8]))

(def deterministic (iterate #(mod (inc %) 100) 1))

(defn turn [[position points] die]
  (let [[vals die] (split-at 3 die)
        n (apply + vals)
        position' (as-> (mod (+ position n) 10) $
                    (if (zero? $) 10 $))]
    [[position' (+ points position')] die]))

(defn winner? [[_ points]]
  (>= points 1000))

(defn play [players die]
  (loop [die die
         turns 1
         [p1 p2] (mapv #(vector % 0) players)]
    (let [[next-p die] (turn p1 die)]
      (if (winner? next-p)
        [next-p p2 turns]
        (recur die
               (inc turns)
               [p2 next-p])))))
(defn part1 []
  (let [[_ [_ points] times] (play players deterministic)]
    (* points (* 3 times))))