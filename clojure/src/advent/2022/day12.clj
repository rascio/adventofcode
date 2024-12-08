(ns advent.2022.day12 
  (:require [advent.core :as a]
            [clojure.data.priority-map :as p]))

(def reader (a/read-input 2022 12))
(def input (reader "example"))

(defn in-range [a b n]
  (<= a n b))

(def points {\S :start \E :end})
(def heights (concat (->> (range (int \a) (int \z))
                          (mapv #(vector (char %) (- (int \a) % -1))))
                     [[0 \A]
                      [(inc (int \z)) \E]]))
(defn find-start-end
  [matrix W H]
  (for [y (range 0 H)
        x (range 0 W)
        :let [n (get-in matrix [y x])]
        :when (contains? points n)]
    [(points n) [x y]]))

(defn get-neighbors
  [matrix n mx my]
  (for [x mx
        y my
        :let [vertex (get-in matrix [y x])]
        :when (some? vertex)]
    (when (<= (int vertex) (inc n))
      [x y])))

(defn parse-input
  [matrix]
  (let [W (count (first input))
        H (count input)
        points (into {} (find-start-end matrix W H))
        graph (for [y (range 0 H)
                    x (range 0 W)
                    :let [node (get-in matrix [y x])
                          connections (concat (->> (get-neighbors matrix
                                                                  (int node)
                                                                  [(dec x) (inc x)]
                                                                  [y])
                                                   (filter some?))
                                              (->> (get-neighbors matrix
                                                                  (int node)
                                                                  [x]
                                                                  [(dec y) (inc y)])
                                                   (filter some?)))]]
                [[x y] connections])]
    {:points points
     :graph (into {} graph)}))

(defn assoc-priority
  [m v p]
  (if (contains? m v)
    (assoc m v p)
    m))
(defn dijsktra
  [graph source]
  (let [dist (->> (keys graph)
                  (mapv #(vector % Integer/MAX_VALUE))
                  (into {}))]
    (loop [dist (assoc dist source 0)
           prev {}
           queue (apply p/priority-map (->> (keys graph)
                                            (mapcat #(vector % (dist %)))))]
      (if (empty? queue)
        [dist prev]
        (let [[vertex _] (first queue)
              alt (inc (dist vertex))
              [dist prev queue] (->> (graph vertex)
                                     (reduce (fn [[dist prev queue] v]
                                               (if (< alt (dist v))
                                                 [(assoc dist v alt)
                                                  (assoc prev v vertex)
                                                  (assoc-priority queue v alt)]
                                                 [dist prev queue]))
                                             [dist prev queue]))]
          (recur dist
                 prev
                 (dissoc queue vertex)))))))

(defn get-number-of-steps
  [{:keys [start end]} [_ prev]]
  (loop [steps 0
         v end]
    (if (= v start)
      steps
      (do (println v)
        (recur (inc steps)
               (prev v))))))
(defn part-1 []
  (let [matrix (to-array-2d input)
        {:keys [points graph]} (parse-input matrix)]
    (->> (dijsktra graph (points :start))
         (get-number-of-steps points))))