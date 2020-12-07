(ns advent.2020.day7
  (:require [advent.core :as a]))

(def reader (a/read-input 2020 7))
(def input (reader))

(def statement-container-regex #"([\s\w]+) bags contain (.*)")
(def statement-containing-regex #"(\d+) ([\s\w]+) bags?[,.]")

(defn parse-statement [statement]
  (let [[_ container-bag containing-statement] (re-find statement-container-regex statement)]
    (->> (re-seq statement-containing-regex containing-statement)
         (map rest)
         (map (a/seq-map a/str->int))
         (cons container-bag))))


;Part1
(defn reversed-graph [parsed]
  (->> parsed
       (mapcat (fn [[from & rels]] 
                 (map (fn [[_ to]] [to from]) rels)))
       (reduce (fn [acc [from to]]
                 (update acc from conj to))
               {})))
(defn find-containers
  ([graph] (find-containers graph (graph "shiny gold")))
  ([graph containers]
   (if (empty? containers)
     nil
     (->> containers
          (mapcat #(->> (find-containers graph (graph %))
                        (cons %)))
          (set)))))

(defn part1 []
  (->> input
       (map parse-statement)
       (reversed-graph)
       (find-containers)
       (count)))


;Part2
(defn ->graph-counters [parsed]
  (->> parsed
       (map (fn [[from & rels]] (->> rels
                                     (vector from))))
       (into {})))
(defn count-bags 
  ([bag graph]
   (let [inner-bags (graph bag)]
     (reduce (fn [acc [n bag']]
               (->> (count-bags bag' graph)
                    (* n)
                    (+ acc n)))
             0
             inner-bags))))

(defn part2 []
  (->> input
       (map parse-statement)
       (->graph-counters)
       (count-bags "shiny gold")))



#_#_ ;Part1 cyclic graph
  (defn ->graph [parsed]
    (->> parsed
         (map (fn [[from & rels]] (->> rels
                                       (map second)
                                       (set)
                                       (vector from))))
         (into {})))
  (defn find-containers [graph]
    (let [bags (->> graph
                    (sort-by (comp count second))
                    (map first))]
      (letfn [(find [[bag & others] res]
                (let [rels (graph bag)]
                  (cond
                      ;end of sequence
                    (nil? bag) res
                      ;already evaluated
                    (contains? res bag) (find others res)
                      ;found!
                    (contains? rels "shiny gold") (->> (assoc res bag true)
                                                       (find others))
                      ;look at the rels (updating the global result)
                    :else (let [res' (find rels res)]
                            (->> (some res' rels)
                                 (assoc res' bag)
                                 (find others))))))]
        (->> (find bags {})
             (filter second)
             (map first)))))
