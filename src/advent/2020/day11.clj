(ns advent.2020.day11
  (:require [advent.core :as a]
            [clojure.string :as s]))

(def reader (a/read-input 2020 11))
(def input (reader "example"))

(def EMPTY \L)
(def OCCUPIED \#)
(def FLOOR \.)

(def directions (for [y [-1 0 1] x [-1 0 1]
                      :let [pos [x y]]
                      :when (not= pos [0 0])]
                     pos))

(defn create-matrix [rows cols]
  (vec (repeatedly rows #(vec (repeat cols 0)))))

(defn parse-seats [lines]
  (let [matrix (->> lines
                    (map #(->> (.toCharArray %)
                               (vec)))
                    (vec))]
    {:matrix matrix 
     :rows (count matrix) 
     :cols (count (first matrix))}))

(defn nearby [{:keys [rows cols]}]
  (fn [next [r c seat]]
    (->> (map #(map + [r c] %) directions)
         (filter (fn [[r' c']] (and (>= (dec rows) r' 0)
                                    (>= (dec cols) c' 0))))
         (reduce (fn [acc [r' c']]
                   (if (= OCCUPIED seat)
                     (update-in acc [r' c'] inc)
                     acc))
                 next))))
(defn update-seat-state [rows cols max-adjacents matrix counters]
  (->>
   (for [r (range rows)
         c (range cols)]
     [r c (get-in counters [r c]) (get-in matrix [r c])])
   (reduce (fn [seats [r c adjacents seat]]
             (->>
              (cond (and (= EMPTY seat)
                         (= 0 adjacents)) OCCUPIED
                    (and (= OCCUPIED seat)
                         (<= max-adjacents adjacents)) EMPTY
                    :else seat)
              (assoc-in seats [r c])))
           matrix)))
(defn step [{:keys [seat-counters max-adjacents]} {:keys [matrix rows cols] :as m}]
  (->> (for [r (range rows) c (range cols)]
         [r c (get-in matrix [r c])])
       (reduce (seat-counters m)
               (create-matrix rows cols))
       (update-seat-state rows cols max-adjacents matrix)
       (assoc {:rows rows :cols cols} :matrix)))

(defn stabilize [opts seats]
  (loop [state seats s 1]
    (let [s' (step opts state)]
      (if (= s' state)
        s'
        (recur s' (inc s))))))

(defn count-seats [{:keys [matrix]}]
  (->> matrix
       (mapcat identity)
       (filter #(= OCCUPIED %))
       (count)))

(defn part1 []
  (->> input
       (parse-seats)
       (stabilize {:seat-counters nearby
                   :max-adjacents 4})
       (count-seats)))


;Part 2
(defn follow [direction start {:keys [rows cols matrix]}]
  (->> (iterate #(map + direction %) start)
       (drop 1)
       (take-while (fn [point] (and (> rows (first point) -1)
                                    (> cols (second point) -1))))
       (drop-while (fn [point] (= FLOOR (get-in matrix point))))
       (take 1)))
(defn visible [matrix]
  (fn [counters [r c seat]]
    (->> directions
         (mapcat #(follow % [r c] matrix))
         (reduce (fn [acc [r' c']]
                   (if (= OCCUPIED seat)
                     (update-in acc [r' c'] inc)
                     acc))
                 counters))))

(defn part2 []
  (->> input
       (parse-seats)
       (stabilize {:seat-counters visible
                   :max-adjacents 5})
       (count-seats)))