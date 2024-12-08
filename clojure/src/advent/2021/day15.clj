(ns advent.2021.day15
  (:require [advent.core :as a]
            [clojure.data.priority-map :refer [priority-map]]))

(def reader (a/read-input 2021 15))

(def input (->> (reader "example")
                (mapv #(mapv a/str->int %))))

(def W (count (first input)))
(def H (count input))

(defn connections [[y x]]
  (vec
   (for [[ix iy] [[-1 0] [0 -1] [1 0] [0 1]]
         :let [x' (+ x ix)
               y' (+ y iy)]
         :when (and (< -1 x' W)
                    (< -1 y' W))]
     [y' x'])))

(defn distance [v dist] (or (first (dist v)) Integer/MAX_VALUE))
(defn dijsktra [start end]
  (loop [[vertex & tail] (for [x (range 0 W)
                               y (range 0 H)]
                           [y x])
         res {start [0 nil]}]
    (println vertex)
    (if (or (nil? vertex) (= vertex end))
      (res end)
      (->> (connections vertex)
           (reduce
            (fn [acc v]
              (let [weight (+ (first (res vertex)) (get-in input v))
                    w (distance v res)]
                (if (> w weight)
                  (assoc acc v [weight vertex])
                  acc)))
            res)
           (recur (sort-by (fn [v] (distance v res)) tail))))))

(defn dijsktra2 [start end]
  (loop [[vertex & tail] (->> (for [x (range 0 W)
                                    y (range 0 H)
                                    :let [p [y x]]]
                                [p (if (= p start) 0 Integer/MAX_VALUE)])
                              (into (priority-map)))
         res {start [0 nil]}]
    (println vertex)
    (if (or (nil? vertex) (= vertex end))
      (res end)
      (let [[graph dist] (->> (connections vertex)
                              (reduce
                               (fn [[g r] v]
                                 (let [weight (+ (first (res vertex)) 
                                                 (get-in input v))
                                       w (distance v res)]
                                   (if (> w weight)
                                     [(assoc g v weight)
                                      (assoc r v [weight vertex])]
                                     [g r])))
                               [tail res]))]
        (recur graph dist)))))

(defn path-matrix [res p]
  (let [m (make-array Boolean/TYPE H W)]
    (loop [[y x :as v] p]
      (aset m y x true)
      (if (= v [0 0])
        m
        (let [[next _] (res v)]
          (recur next))))
    (doseq [row m]
      (->> (map #(if % "X" " ") row)
           (apply str)
           (println)))))
(defn part1 []
  (dijsktra [0 0] [(dec H) (dec W)])
  #_(-> (min-risk [0 0] [(dec H) (dec W)])
      (get [(dec H) (dec W)])))