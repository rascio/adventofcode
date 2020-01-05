(ns advent.2019-day2
    (:require [advent.core :as a]))

(def reader (a/read-input 2019 2))

(def input (reader))

(def operations {1 + 2 *})

(defn compute 
    [instructions]
    (let [state (atom instructions)]
        (loop [index 0]
            (let [[op arg0 arg1 pos] (subvec @state 
                                             index 
                                             (+ index 4))
                    operator (operations op)]
                (when operator
                    (->> (operator (nth @state arg0) 
                                   (nth @state arg1))
                         (swap! state assoc pos)))
                (if (= 99 op)
                    @state
                    (recur (+ index 4)))))))

(defn part-1 []
  (let [res (->> input
                (mapcat #(clojure.string/split % #","))
                (map a/str->int)
                (vec)
                (compute)
                (clojure.string/join ","))]
    (println res)))

(defn set-noun-verb [n v i]
    (assoc i 1 n 2 v))

(defn part-2 []
    (let [instructions (->> input
                           (mapcat #(clojure.string/split % #","))
                           (map a/str->int)
                           (vec))]
        (first
            (for [n (range 0 99)
                  v (range 0 99)
                  :let [program (set-noun-verb n v instructions)
                        [out & _] (compute program)]
                  :when (= 19690720 out)]
                (+ (* 100 n) v)))))

