(ns advent.2021.day4
  (:require [advent.core :as a]))

(def example false)
(def reader (a/read-input 2021 4))
(def input (->> (if example
                  (reader "example")
                  (reader))
                (reduce
                 (fn [[state numbers [b & boards]] line]
                   (case state
                     :init (if (empty? line)
                             [:board numbers []]
                             (as-> line $
                               (.split $ ",")
                               (map a/str->int $)
                               [state $ []]))
                     :board (if (empty? line)
                              [state numbers (concat [[] b] boards)]
                              (as-> line $
                                (re-seq #"\d+" $)
                                (mapv a/str->int $)
                                (conj b $)
                                [state numbers (cons $ boards)]))))
                 [:init])))

(defn explode-board 
  [board]
  (->> board
       (apply map vector)
       (concat board)))
(defn check-number [n]
  (let [f (complement #{n})]
    (fn [board] 
      (map #(filter f %) board))))
(defn find-winner 
  [[_ numbers boards]]
  (reduce
   (fn [boards number]
     (let [remove-number (check-number number)
           result (reduce
                   (fn [acc board]
                     (as-> board $
                       (remove-number $)
                       (if (some empty? $)
                         (reduced {:winner $})
                         (cons $ acc))))
                   (list)
                   boards)]
       (if (map? result)
         (->> (:winner result)
              (take 5)
              (conj [number])
              (reduced))
         result)))
   (map explode-board boards)
   numbers))
(defn calc-result
  [[n winner]]
  (->> (flatten winner)
       (reduce +)
       (* n)))
(defn part1 []
  (->> input
       (find-winner)
       (calc-result)))

; Part2

(defn find-last-winner
  [[_ numbers boards]]
  (reduce
   (fn [boards number]
     (let [remove-number (check-number number)
           result (reduce
                   (fn [acc board]
                     (as-> board $
                       (remove-number $)
                       (if (some empty? $)
                         acc
                         (cons $ acc))))
                   (list)
                   boards)]
       (if (empty? result)
         (->> (first boards)
              (remove-number)
              (take 5)
              (conj [number])
              (reduced))
         result)))
   (map explode-board boards)
   numbers))
(defn part2 []
  (->> input
       (find-last-winner)
       (calc-result)))