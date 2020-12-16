(ns advent.2020.day13
  (:require [advent.core :as a]
            [math :as m]))

(def reader (a/read-input 2020 13))
(def input (reader "example"))

(defn parse [[id bus]]
  [(a/str->int id)
   (->> (.split bus ",")
        (mapv #(if (= "x" %)
                 :suppressed
                 (a/str->int %))))])

(defn part1 []
  (let [[depart buses] (parse input)
        [bus wait] (->> buses
                        (filter #(not= :suppressed %))
                        (map (fn [b] [b (- b (mod depart b))]))
                        (sort-by second)
                        (first))
        timestamp (+ depart wait)]
    (* bus (- timestamp depart))))

(defn same-schedule [buses]
  (loop [[b & tail] buses
         idx 0
         acc ()]
    (cond (nil? b) (reverse acc)
          (number? b) (let [same-schedule (nth tail (dec b) nil)]
                        (->> (if (number? same-schedule)
                               (cons [idx b same-schedule] acc)
                               acc)
                             (recur tail 1)))
          :else (recur tail (inc idx) acc))))

(defn find-first [p coll]
  (reduce (fn [_ v]
            (cond (p v)
              (reduced v)))
          nil
          coll))
(defn part2 []
  (let [[_ buses] (parse input)
        schedules (->> buses
                       (map-indexed (fn [idx b] (when (number? b)
                                                  [idx b])))
                       (filter some?))]
    (println "schedules:" schedules)))

(defn modulo [a b r]
  (loop [acc r]
    (if (and (> r 0) (= 0 (mod acc b)))
      acc
      (recur (+ acc a)))))

(defn solve
  ([[i a] [i' b] & tail]
   (if (nil? tail)
     (modulo a b (- i' i))
     (+ (modulo a b (- i' i)) (apply solve [i' b] tail)))))

(defn mega-brute-force [[_ v :as head] & tail]
  (->> tail
       (map #(vector (first %) 
                     (modulo v (second %) (first %))
                     (* v (second %))))))