(ns advent.2021.day3
  (:require [advent.core :as a]))

(def reader (a/read-input 2021 3))
(def input (reader))

(defn binary->int [n]
  (->> (reverse n)
       (vec)
       (reduce-kv
        (fn [acc idx v]
          (+ acc (bit-shift-left v idx)))
        0)))
(defn str->binary [line]
  (mapv #(if (= \0 %) 0 1) line))

(def safe-inc (fnil inc 0))
(defn common-bits [measures]
  (->> measures
       (reduce
        (partial
         reduce-kv
         (fn [acc idx digit]
           (update-in acc [idx digit] safe-inc)))
        {})
       (sort-by first)))
(defn part1 []
  (->> input
       (map str->binary)
       (common-bits)
       (reduce
        (fn [acc [_ vals]]
          (let [v (->> (sort-by second vals)
                       (map first))]
            (-> acc
                (update :gamma conj (second v))
                (update :epsilon conj (first v)))))
        {:gamma []
         :epsilon []})
       (map #(binary->int (second %)))
       (apply *)))

(defn oxygen-criteria
  [[d v] [d' v']]
  (compare [v' d'] [v d]))
(defn co2-criteria
  [[d v] [d' v']]
  (compare [v d] [v' d']))
(defn bit-criteria
  ([criteria measures]
   (bit-criteria criteria measures 0))
  ([criteria measures idx]
   (if (= 1 (count measures))
     (first measures)
     (let [bit (->> measures
                    (map #(nth % idx))
                    (frequencies)
                    (sort criteria)
                    (first)
                    (first))]
       (recur criteria
              (filter #(= bit (nth % idx)) measures)
              (inc idx))))))
(defn part2 []
  (let [measures (map str->binary input)
        oxygen   (bit-criteria oxygen-criteria measures)
        co2      (bit-criteria co2-criteria measures)]
    (->> [oxygen co2]
         (map binary->int)
         (apply *))))