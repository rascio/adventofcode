(ns advent.2021.day10
  (:require [advent.core :as a]))

(def reader (a/read-input 2021 10))
(def input (reader))

(def error-points { \) 3, \] 57\} 1197, \> 25137 })
(def closing { \( \), \[ \], \{ \}, \< \> })

(defn syntax-score
  [line]
  (loop [[current & closing-tokens :as expected] nil
         [check & remaining] line]
    (if (nil? check)
      [:correct expected]
      (if (= check current)
        (recur closing-tokens remaining)
        (if-let [next-closing-token (closing check)]
          (recur (conj expected next-closing-token)
                 remaining)
          [:error check])))))

(defn part1 []
  (->> input
       (map syntax-score)
       (filter #(= :error (first %)))
       (map second)
       (map error-points)
       (reduce +)))



(defn success-points
  [tokens]
  (reduce
   (fn [points c]
     (-> points
         (* 5)
         (+ (case c
              \) 1
              \] 2
              \} 3
              \> 4))))
   0
   tokens))

(defn median
  [scores]
  (-> scores
      (sort)
      (nth (/ (count scores) 2))))

(defn part2 []
  (->> input
       (map syntax-score)
       (filter #(= :correct (first %)))
       (map second)
       (map success-points)
       (median)))