(ns advent.2021.day18
  (:require [advent.core :as a]))

(def log (a/debugger false))
(def reader (a/read-input 2021 18))

(def input (->> (reader)
                (map read-string)))

(defprotocol SnailfishNumber
  (merge [n v direction])
  (explode [n depth])
  (split [n])
  (magnitude [n]))

(defn reduce-n [n]
  (loop [result n]
    ;(a/debug-forms result)
    (if-let [exploded (:value (explode result 0))]
      (do (a/log-forms log exploded)
          (recur exploded))
      (if-let [splitted (split result)]
        (do (a/log-forms log splitted)
            (recur splitted))
        result))))
(defn sum [a b] (reduce-n [a b]))

(extend clojure.lang.Seqable
  SnailfishNumber
  {:merge (fn [[x y] v direction]
            (case direction
              :left [(merge x v :left) y]
              :right [x (merge y v :right)]))
   :explode (fn [[x y] depth]
              ;(println (a/pad-left depth ">") "x:" x "y:" y "depth:" depth)
              ;(a/pdebug (a/pad-left depth ">"))
              (or (when-let [v (explode x (inc depth))]
                    {:left (v :left)
                     :value [(v :value) (merge y (v :right) :left)]
                     :right 0})
                  (when-let [v (explode y (inc depth))]
                    {:left 0
                     :value [(merge x (v :left) :right) (v :value)]
                     :right (v :right)})
                  (when (<= 4 depth)
                    {:left x :value 0 :right y})))
   :split (fn [[x y]] (or (when-let [a (split x)]
                            [a y])
                          (when-let [b (split y)]
                            [x b])))
   :magnitude (fn [[x y]] (+ (* 3 (magnitude x)) (* 2 (magnitude y))))})

(extend java.lang.Number
  SnailfishNumber
  {:merge (fn [n v _] (+ n v))
   :explode (fn [_ _] nil)
   :split (fn [n]
            (if (>= n 10)
              [(int (Math/floor (/ n 2))) (int (Math/ceil (/ n 2)))]
              nil))
   :magnitude identity})


(defn part1 []
  (magnitude (reduce sum input)))

(defn part2 []
  (->>
   (for [a input]
     (for [b input
           :when (not= a b)]
       (magnitude (sum a b))))
   (mapcat identity)
   (sort)
   (last)))