(ns advent.2021.day13
  (:require [advent.core :as a]))

(def reader (a/read-input 2021 13))

(def input (->> (reader)
                (reduce
                 (fn [[state points folds] line]
                   (case state
                     :init (if (empty? line)
                             [:fold points folds]
                             (as-> line $
                               (a/re-map #"(\d+),(\d+)" [a/str->int a/str->int] $)
                               (conj points $)
                               [state $ folds]))
                     :fold (as-> line $
                             (a/re-map #"fold along (\w+)=(-?\d+)" [keyword a/str->int] $)
                             (conj folds $)
                             [state points $])))
                 [:init #{} []])
                (rest)))
(defn fold
  [points [axe v]]
  (reduce
   (fn [acc [x y]]
     (->>
      (case axe
        :x (if (> x v) [(- v (- x v)) y] [x y])
        :y (if (> y v) [x (- v (- y v))] [x y]))
      (conj acc)))
   #{}
   points))
(defn print-points
  [points]
  (let [[max-x max-y] (reduce
                       (fn [[mx my] [x y]] [(max mx x) (max my y)])
                       points)]
    (->> (range 0 (inc max-y))
         (map (fn [y]
                (->> (range 0 (inc max-x))
                     (map #(if (points [% y]) "#" " "))
                     (reduce str))))
         (map println)
         (dorun))))
(defn part1 []
  (let [[points folds] input]
    (->>
     (first folds)
     (fold points)
     (count))))
(defn part2 []
  (let [[points folds] input]
    (->>
     (reduce fold points folds)
     (print-points))))