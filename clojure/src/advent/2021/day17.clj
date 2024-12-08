(ns advent.2021.day17
  (:require [advent.core :as a]))

(def reader (a/read-input 2021 17))

(def input (->> (reader "example")
                (map (fn [line]
                       (let [[x xe y ye] (->> line
                                                (re-matches #"target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)")
                                                (rest)
                                                (map a/str->int))]
                         [[x xe] [ye y]])))
                (first)))

(defn dec-speed [xs]
  (cond
    (zero? xs) 0
    (pos? xs) (dec xs)
    :else (inc xs)))

(defn is-in? [[x y] [[ax ax'] [ay ay']]]
  (and (<= ax x ax')
       (>= ay y ay')))

(defn step [[[x y] [xs ys]]]
  [[(+ x xs) (+ y ys)] [(dec-speed xs) (dec ys)]])

(defn valid-speed? [[xs ys] [[_ ax'] [_ ay'] :as area]]
  (->> (iterate step [[0 0] [xs ys]])
       (map first)
       (take-while
        (fn [[x y]] 
          (and (<= x ax')
               (>= y ay'))))
       (filter 
        (fn [p] (is-in? p area)))
       (first)))
