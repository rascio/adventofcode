(ns advent.2020.day9
  (:require [advent.core :as a]))

(def reader (a/read-input 2020 9))
(def input (reader))
(def preamble 25) ;25 or 5 (if example)

;Part 1
(defn checksum [target list]
  (loop [[head & tail] list]
    (cond
      (nil? tail) false
      (some #(= target (+ head %)) tail) true
      :else (recur tail))))

(defn check-xmas [n numbers]
  (let [top (take n numbers)
        to-check (nth numbers n)]
    (if (checksum to-check top)
      (->> (rest numbers)
           (check-xmas n))
      to-check)))

(defn part1 []
  (->> input
       (map a/str->long)
       (check-xmas preamble)))


;Part 2
#_(def target 14144619)
(def target (part1))

(defn break-xmas [[n & numbers]]
  (let [check (loop [[n' & nums] numbers
                     acc n
                     res [n]]
                (cond
                  (nil? n') nil
                  (< target (+ acc n')) nil
                  (= target (+ acc n')) (cons n' res)
                  :else (recur nums (+ acc n') (cons n' res))))]
    (if (nil? check)
      (break-xmas numbers)
      check)))

(defn part2 []
  (let [contiguous-set (->> input
                            (map a/str->long)
                            (break-xmas))]
    (+ (apply min contiguous-set)
       (apply max contiguous-set))))