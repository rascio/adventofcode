(ns advent.2022.day9
  (:require [advent.core :as a]))

(def reader (a/read-input 2022 9))
(def input (reader))

(defn sum [[ax ay] [bx by]]
  [(+ ax bx) (+ ay by)])
(defn diff [[ax ay] [bx by]]
  [(- ax bx) (- ay by)])
(defn distance
  ([a b]
   (let [d (diff a b)]
     (distance d)))
  ([[x y]]
   (Math/sqrt (+ (* x x) (* y y)))))


(defn bounded [n]
  (max -1 (min 1 n)))
(defn follow [head tail]
  (let [[dx dy :as d] (diff head tail)]
    (if (>= (distance d) 2)
      (sum tail [(bounded dx) (bounded dy)])
      tail)))
  
(defn step
  [head tail direction]
  (let [head (sum head direction)]
    [head (follow head tail)]))

(def directions {\R [1 0]
                 \L [-1 0]
                 \U [0 1]
                 \D [0 -1]})

(def command-regex #"(\w) (\d+)")
(defn parse-commands [input]
  (->> input
       (mapcat (fn [line] 
                 (let [[cmd times] (a/re-map command-regex
                                             [(comp directions first) a/str->int]
                                             line)]
                   (repeat times cmd))))))

(defn debug-step [w h knots]
  (let [knots (->> knots
                   (map-indexed (fn [idx v] [v idx]))
                   (sort-by first)
                   (reverse)
                   (into {}))]
    (doseq [y (reverse h)]
      (doseq [x w]
        (print (get knots [x y] ".")))
      (println))))
(defn part-1 []
  (->> input
       (parse-commands)
       (reduce (fn [[[head tail] acc] cmd]
                 (let [[head tail] (step head tail cmd)]
                   #_(do
                       (a/debug-forms cmd)
                       (debug-step (range 6) (range 5) [head tail])
                       (println))
                   [[head tail] (conj acc tail)]))
               [[[0 0] [0 0]] #{[0 0]}])
       (last)
       (count)))


(defn part-2 []
  (->> input
       (parse-commands)
       (reduce
        (fn [[[head & knots] acc] cmd]
          (let [new-head (sum head cmd)
                knots (->> knots
                           (reduce (fn [[head acc] k]
                                     (let [k (follow head k)]
                                       [k (conj acc k)]))
                                   [new-head [new-head]])
                           (second))
                tail (last knots)]
            #_(do
                (a/debug-forms cmd knots)
                (debug-step (range -13 13) (range -5 16) knots)
                (println))
            [knots (conj acc tail)]))
        [(repeat 10 [0 0]) #{[0 0]}])
       (last)
       (count)))