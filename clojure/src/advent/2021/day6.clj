(ns advent.2021.day6
  (:require [advent.core :as a]))

(def reader (a/read-input 2021 6))
(def input (->> (reader)
                (mapcat #(re-seq #"\d+" %))
                (mapv a/str->int)))

(defn part1 []
  (loop [days 80
         lanternfishes input]
    (if (= 0 days)
      (->> lanternfishes
           (count))
      (->> lanternfishes
           (mapcat
            (fn [days]
              (if (= 0 days)
                [6 8]
                [(dec days)])))
           (recur (dec days))))))

(def zeros (vec (repeat 9 0)))
(defn part2 []
  (loop [steps 256
         lanternfishes (reduce
                        (fn [acc day] (update acc day + 1))
                        zeros
                        input)]
    (if (= 0 steps)
      (reduce + lanternfishes)
      (->> lanternfishes
           (reduce-kv
            (fn [next day v]
              (if (= 0 day)
                (-> next
                    (update 6 + v)
                    (update 8 + v))
                (update next (dec day) + v)))
            zeros)
           (recur (dec steps))))))