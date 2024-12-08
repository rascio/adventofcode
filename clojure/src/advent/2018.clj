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
         (fn [[_ id]] {:type :start :date date :id (Integer/parseInt id)})
    #"falls asleep" :>>
         (fn [_] {:type :asleep :date date})
    #"wakes up" :>>
         (fn [_] {:type :awake :date date})
    {:type :invalid :txt txt}))

(def log-time-format (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm"))

(defn format
  [time format]
  (.format time (DateTimeFormatter/ofPattern format)))

(defn log->event
  [log]
  (let [[log-time log-message] (re-extract #"^\[(.+?)\] (.+?)$" log)
        time (LocalDateTime/parse log-time log-time-format)]
    (guard-event time log-message)))

(defn logs->events
  [logs]
  (->> logs
    (group-by :type)
    (reduce
      (fn [m [k [v]]] (assoc m k v))
      {})))

(defn range->minutes
  [start stop]
  (reduce
    (fn [res time]
      (let [idx    (format time "dd-MM-yyyy")
            minute (format time "HH:mm")]
        (assoc res idx (cons minute (get res idx '())))))
    {}
    (->> (iterate #(.plusMinutes % 1) start)
         (take-while (comp pos? (partial compare stop))))))

(defn events->timetable
  [e]
  (loop [[event & others] e
         last nil
         res {}]
    (if (nil? event)
      res
      (case (event :type)
        :awake (let [from     (last :date)
                     id       (last :id)
                     to       (event :date)
                     minutes  (range->minutes from to)]
                 (recur others event
                   (update res id
                     (partial merge-with clojure.set/union minutes))))
        (recur others event res)))))

(defn timetable->stats
  [timetable]
  (reduce
    (fn [res [id days]]
      (reduce
        (fn [r [day minutes]]
          (reduce
            #(assoc-in %1 [id %2] (inc (get-in %1 [id %2] 0)))
            r
            minutes))
        res
        days))
    {}
    timetable))

(defn mn->int
  [minute]
  (Integer/parseInt (second (re-matches #"\d\d:(\d\d)" minute))))


(a/defcase day4
  [input "2018/4.input.txt"]
  (let [
        events (->> input
                 (sort)
                 (map log->event))
        timetable (events->timetable events)
        stats (timetable->stats timetable)
        [guard-id
         guard-stats] (->> stats
                        (sort-by
                          (comp
                            (partial reduce + 0)
                            (partial map second)
                            second)
                          >)
                        first)
        best-minute (first (sort-by second > guard-stats))]
    (println "Result:" guard-id best-minute (* guard-id (mn->int (key best-minute))))))


(defn most-sleeped-minute
  [stats]
  (->> stats
    (reduce
      (partial merge-with +)
      {})
    (sort-by second >)
    (first)
    (key)
    (mn->int)))


(a/defcase day4-part2
  [input "2018/4.input.txt"]
  (let [
        events (->> input
                 (sort)
                 (map log->event)
                 (sort-by :date))
        timetable (events->timetable events)
        stats (timetable->stats timetable)
        result (->> stats
                 (map
                  (fn [[id & days]]
                     {:id id
                       :minute (most-sleeped-minute days)}))
                 (sort-by :minute >)
                 (first))]
    (println "Result:" result (* (result :id) (result :minute)))))
