(ns advent.2022.day11 
  (:require [advent.core :as a]
            [clojure.pprint :as pp]))

(def reader (a/read-input 2022 11))
(def input (reader "example"))

(defn parse-operation-value
  [value]
  (if (= "old" value)
    identity
    (constantly (a/str->int value))))
(def operation-functions
  {"+" +
   "*" *})
(defn parse-operation 
  [line]
  (let [[a op b] (a/re-map #"\s*Operation: new = (\w+) (\+|\*) (\w+)"
                           [parse-operation-value operation-functions parse-operation-value]
                           line)]
    (fn [n]
      (op (a n) (b n)))))

(defn parse-test
  [test [if-true if-false & input]]
  (let [test (->> test
                  (re-find #"\d+")
                  (a/str->int))
        if-true (->> if-true
                     (re-find #"\d+")
                     (a/str->int))
        if-false (->> if-false
                      (re-find #"\d+")
                      (a/str->int))
        get-monkey (fn [n]
                     (if (= 0 (mod n test))
                       if-true
                       if-false))]
    [get-monkey input]))
(defn parse-monkey
  [input]
  (loop [[line & input] input
         state :init
         monkey {}]
    (case state
      :done [monkey input]
      :init (recur input
                   :items
                   (assoc monkey :name line))
      :items (recur input
                    :operation
                    (->> line
                         (re-seq #"\d+")
                         (mapv bigint)
                         (assoc monkey :items)))
      :operation (recur input
                        :test
                        (->> (parse-operation line)
                             (assoc monkey :operation)))
      :test (let [[test input] (parse-test line input)]
              (recur input
                     :done 
                     (assoc monkey :test test))))))

(defn parse-monkeys
  [input]
  (loop [input input
         monkeys []]
    (if (not-empty input)
      (let [[m input] (parse-monkey input)]
        (recur input
               (conj monkeys m)))
      monkeys)))

(defn next-worry-level
  [{:keys [operation]} worry-level]
  (operation worry-level))
(defn throw-to-monkey
  [{:keys [test]} worry-level]
  (test worry-level))
(def three (bigint 3))
(defn div3
  [n]
  (.quotient n three))

(defn turn
  [monkey calculate-next-worry-level]
  (for [item (monkey :items)
        :let [worry-level (calculate-next-worry-level monkey item)
              next-monkey-idx (throw-to-monkey monkey worry-level)]]
    [next-monkey-idx worry-level]))

(defn round
  [monkeys inspections calculate-next-worry-level]
  (loop [idx 0
         inspections inspections
         monkeys monkeys]
    (if (= idx (count monkeys))
      [monkeys inspections]
      (let [monkey (nth monkeys idx)
            items-thrown (turn monkey calculate-next-worry-level)]
        (recur (inc idx)
               (update inspections idx + (count items-thrown))
               (reduce (fn [acc [to-monkey item]]
                         (update-in acc [to-monkey :items]
                                    conj item))
                       (assoc-in monkeys [idx :items] [])
                       items-thrown))))))

(defn part-1 []
  (let [monkeys (parse-monkeys input)
        inspections (vec (repeat (count monkeys) 0))
        rounds (range 20)
        calculate-next-worry-level (comp div3 next-worry-level)
        [_ inspections] (reduce (fn [[monkeys inspections] _]
                                  (round monkeys inspections calculate-next-worry-level))
                                [monkeys inspections]
                                rounds)]
    (->> inspections
         (sort >)
         (take 2)
         (apply *))))


(defn part-2 []
  (let [monkeys (parse-monkeys input)
        inspections (vec (repeat (count monkeys) 0))
        rounds (range 1000)
        [_ inspections] (reduce (fn [[monkeys inspections] _]
                                  (round monkeys inspections next-worry-level))
                                [monkeys inspections]
                                rounds)]
    inspections))