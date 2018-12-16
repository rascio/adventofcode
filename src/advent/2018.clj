(ns advent.2018
  (:require [advent.core :as a]
            [clojure.core.reducers :as r])
  (:import (java.time
             LocalDateTime
             format.DateTimeFormatter)))

;https://adventofcode.com/2018/day/1
(a/defcase day1
  [input "2018/1.input.txt"]
  (reduce +
      (map #(Integer/parseInt %) input)))



(a/defcase day1-part2
  [input "2018/1.input.txt"]
  (let [seq (cycle
              (map #(Integer/parseInt %) input))]
    (loop [[n & tail] seq
           tot 0
           history #{}]
      (let [res (+ n tot)]
        (if (contains? history res)
          res
          (recur tail res (conj history res)))))))





;https://adventofcode.com/2018/day/2
; utils
(defn sum-pair
  ([] [0 0])
  ([p] p)
  ([[p1 p2] [m1 m2]]
   [(+ p1 m1) (+ p2 m2)]))

(defn bool->int [b] (if b 1 0))


(a/defcase day2
  [input "2018/2.input.txt"]
  (let [[twos threes]
        (transduce
          (comp
            (map seq)
            (map frequencies)
            (map vals)
            (map set)
            (map (juxt
                   #(bool->int (contains? % 2))
                   #(bool->int (contains? % 3)))))
          sum-pair
          input)]
    (* twos threes)))



;utils
(defn diff
  ([seq1 seq2]
   (->> (map vector seq1 seq2)
        (map (fn [[s1 s2]] (not= s1 s2))))))


(a/defcase day2-part2
  [input "2018/2.input.txt"]
  (loop [[word & words] (map seq input)]
    (let [diff-char (->> words
                      (map #(diff word %))
                      (filter #(= 1 (count (filter identity %))))
                      first)]
      (if (not (nil? diff-char))
        ;word found, now get the common part
        (->> (map vector word diff-char)
             (filter (comp not second))
             (map first)
             (apply str))
        (recur words)))))





;https://adventofcode.com/2018/day/3
;utils
(defn re-extract
  [regex txt] (rest (re-find regex txt)))

(defn rect
  [txt]
  (let [[id x y width height]
        (re-extract #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" txt)]
    {
      :id id
      :x (Integer/parseInt x)
      :y (Integer/parseInt y)
      :x2 (+ (Integer/parseInt x) (Integer/parseInt width))
      :y2 (+ (Integer/parseInt y) (Integer/parseInt height))}))

(defn matrix
  [w h]
  (->> (repeat h w)
       (map #(vec (repeat % 0)))
       (into [])))

(defn map-point
  [f matrix point]
  (assoc-in matrix point (f (get-in matrix point))))


(a/defcase day3
  [input "2018/3.input.txt"]
  (let [rects (set (map #(rect %) input))
        width (apply max (map :x2 rects))
        height (apply max (map :y2 rects))]
    (->> rects
      (reduce ;to a matrix with counters
        (fn [area rect]
          (reduce ;the points of the rectangle in counters
            (partial map-point inc)
            area
            (for [x (range (:x rect) (:x2 rect))
                  y (range (:y rect) (:y2 rect))]
              [x y])))
        (matrix width height))
      (flatten)
      (filter (partial < 1))
      (count))))



(a/deflambda not-overlap?
  [r1 r2]
  (or
    (>= (:x r1) (:x2 r2))
    (<= (:x2 r1) (:x r2))
    (>= (:y r1) (:y2 r2))
    (<= (:y2 r1) (:y r2))))


(a/defcase day3-part2
  [input "2018/3.input.txt"]
  (let [rects (set (map rect input))]
    (filter
      (fn [rect]
        (every? (not-overlap? rect) (disj rects rect)))
      rects)))



;https://adventofcode.com/2018/day/4
(defn guard-event
  [date txt]
  (condp re-matches txt
    #"Guard #(\d+) begins shift" :>>
         (fn [[_ id]] {:type :start :date date :id id})
    #"falls asleep" :>>
         (fn [_] {:type :asleep :date date})
    #"wakes up" :>>
         (fn [_] {:type :awake :date date})
    {:type :invalid :txt txt}))


(defn log->event
  [log]
  (let [[log-time log-message] (re-extract #"^\[(.+?)\] (.+?)$" log)
        time (LocalDateTime/parse log-time log-time-format)]
    (guard-event time log-message)))

(def log-time-format (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm"))

(defn to-idx
  [events time]
  [(-> events :start :id)
   (.getYear time)
   (.getMonth time)
   (.getDayOfMonth time)
   (.getHour time)
   (.getMinute time)])

(defn logs->events
  [logs]
  (->> logs
    (group-by :type)
    (reduce
      (fn [m [k [v]]] (assoc m k v))
      {})))

(defn events->timetable
  [events]
  (reduce
    (fn [res time]
      (let [idx (to-idx events time)]
        (assoc-in res idx (inc (get-in res idx 0)))))
    {}
    (->> (iterate #(.plusMinutes % 1) (-> events :asleep :date))
         (take-while (comp not neg? (partial compare (-> events :awake :date)))))))


(a/defcase day4
  [input "2018/4.mock.txt"]
  (->> input
    (map log->event)
    (sort-by :date)
    (partition 3)
    (map logs->events)
    (map events->timetable)))
