(ns advent.2019
    (:require [advent.core :as a]))

(defn str->int [s] (Integer/parseInt s))

;Day1
(a/defcase day1
    [input "2019/day1.txt"]
    (->> input
        (map (fn [e] (-> (Integer/parseInt e)
                         (/ 3)
                         (int)
                         (- 2))))
        (reduce +)))

(defn fuel [mass]
    (let [f (-> mass 
                (/ 3)
                (int)
                (- 2))]
        (if (neg? f)
            0
            (+ f (fuel f)))))
(a/defcase day1-2
    [input "2019/day1.txt"]
    (->> input
        (map (comp fuel str->int))
        (reduce +)))


;Day2
(def operations {1 + 2 *})
(defn compute [instructions]
    (let [state (atom instructions)]
        (loop [index 0]
            (let [[op arg0 arg1 pos] (subvec @state index (+ index 4))
                   operator (operations op)]
                (when operator
                    (->> [(nth @state arg0) (nth @state arg1)]
                         (apply operator)
                         (swap! state assoc pos)))
                (if (= 99 op)
                    @state
                    (recur (+ index 4)))))))

(a/defcase day2
    [input "2019/day2.txt"]
    (let [res (->> input
                   (mapcat #(clojure.string/split % #","))
                   (map str->int)
                   (vec)
                   (compute)
                   (clojure.string/join ","))]
        (println res)))

(defn set-noun-verb [n v i]
    (assoc i 1 n 2 v))
(a/defcase day2-2
    [input "2019/day2.txt"]
    (let [instructions (->> input
                           (mapcat #(clojure.string/split % #","))
                           (map str->int)
                           (vec))]
        (first
            (for [n (range 0 99)
                  v (range 0 99)
                  :let [program (set-noun-verb n v instructions)
                        [out] (compute program)]
                  :when (= 19690720 out)]
                (+ (* 100 n) v)))))

(a/defcase day-3
    [input "2019/day3.txt"]
    (comment to-do))
