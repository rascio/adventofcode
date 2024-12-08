(ns advent.2019.day6
    (:require [advent.core :as a]))

(def reader (a/read-input 2019 6))
(def debug (a/debugger false))

(def input (reader))

(defn count-parents [graph k]
    (let [parent (graph k)]
        (if (some? parent)
            (inc (count-parents graph parent))
            0)))

(defn pair->rel [graph [f t]]
   (assoc graph t f))

(defn count-orbits [graph]
   (->> (keys graph) 
        (map #(count-parents graph %))
        (reduce +)))

(defn part-1 []
   (->> (map (partial re-find #"(\w+)\)(\w+)") input)
        (map (fn [[_ f t]] [(keyword f) (keyword t)]))
        (reduce pair->rel {})
        (count-orbits)))

(defn hierarchy [graph k]
   (let [parent (graph k)]
      (if (some? parent)
          (cons k (hierarchy graph parent))
          (list k))))

(defn orbits-between [from to graph]
   (let [f (hierarchy graph from)
         t (hierarchy graph to)
         common (->> [(set f) (set t)]
                     (apply clojure.set/intersection)
                     (count))]
      (+ (- (count f) common 1)
         (- (count t) common 1))))

(defn part-2 []
   (->> (map (partial re-find #"(\w+)\)(\w+)") input)
        (map (fn [[_ f t]] [(keyword f) (keyword t)]))
        (reduce pair->rel {})
        (orbits-between :YOU :SAN)))