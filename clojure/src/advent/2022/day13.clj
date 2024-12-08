(ns advent.2022.day13 
  (:require [advent.core :as a]))

(def reader (a/read-input 2022 13))
(def input (reader "example"))

(defn parse-input
  [input]
  (let [[last acc] (->> input
                        (reduce (fn [[last acc] line]
                                  (if (empty? line)
                                    [[] (conj acc last)]
                                    [(conj last (read-string line)) acc]))
                                [[] []]))]
    (conj acc last)))

(defn compare-message
  [left right]
  (a/debug-forms left right)
  (cond
    (and (number? left)
         (number? right)) (cond
                            (< left right) :lt
                            (= left right) :eq
                            :else :gt)
    (and (seqable? left)
         (seqable? right)) (loop [[l & left] left
                                  [r & right] right]
                             (cond
                               (nil? l) :lt
                               (nil? r) :gt 
                               :else (let [c (compare-message l r)] 
                                       (if (= :eq c)
                                         (recur left right)
                                         c))))
    (number? left) (compare-message [left] right)
    (number? right) (compare-message left [right])
    :else :not-managed))

(defn part-1 []
  (->> input
       (parse-input)
       (map-indexed (fn [idx [left right]]
                      [(inc idx) (a/debug compare-message left right)]))
       (filter (fn [[_ res]]
                 (= :lt res)))
       (map first)
       (apply +)))