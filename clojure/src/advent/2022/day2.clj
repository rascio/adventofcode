(ns advent.2022.day2
  (:require [advent.core :as a]))


(def reader (a/read-input 2022 2))
(def input (reader))

(def values {"A" :rock
             "B" :paper
             "C" :scissors
             "X" :rock
             "Y" :paper
             "Z" :scissors})
(def points {:rock 1
             :paper 2
             :scissors 3})
(def outcomes (->> (concat (for [s [:rock :paper :scissors]] [[s s] 3])
                           {[:rock :paper] 0
                            [:paper :scissors] 0
                            [:scissors :rock] 0}
                           {[:paper :rock] 6
                            [:scissors :paper] 6
                            [:rock :scissors] 6})
                   (into {})))
(defn calculate-points [[opponent i]]
  (+ (points i)
     (outcomes [i opponent])))

(defn part-1 []
  (->> input
       (mapcat (fn [v] (. v split " ")))
       (map values)
       (partition 2)
       (map calculate-points)
       (apply +)))

(def reverse-index (->> outcomes
                        (map (fn [[[i opponent] v]]
                               [opponent {v i}]))
                        (reduce (fn [acc [o e]]
                                  (update acc o merge e))
                                {})))
(def outcome-mapping {"X" 0
                      "Y" 3
                      "Z" 6})
(defn calculate-points-2 [[opponent outcome]]
  (+ (points (get-in reverse-index [opponent outcome]))
     outcome))
(defn part-2 []
  (->> input
       (map (fn [v] (. v split " ")))
       (map (fn [[opponent outcome]] 
              [(values opponent) (outcome-mapping outcome)]))
       (map calculate-points-2)
       (apply +)))