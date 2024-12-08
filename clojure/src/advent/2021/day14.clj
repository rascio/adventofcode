(ns advent.2021.day14
  (:require [advent.core :as a]))

(def reader (a/read-input 2021 14))

(let [[template & [_ & mappings]] (reader)]
  (def template template)
  (def mappings (->> mappings
                     (map #(rest (re-matches #"(\w\w) -> (\w)" %)))
                     (reduce (fn [acc [k v]] (assoc acc k (first v))) {}))))
(def results (atom {}))
(defn expand-polymer
  [n polymer]
  #_(println n polymer)
  (if (= 1 n)
    (frequencies (str polymer (mappings polymer)))
    (or
      (get-in @results [polymer n])
      (let [[a b] polymer
            gen (mappings polymer)
            res (-> (merge-with +
                                (expand-polymer (dec n) (str a gen))
                                (expand-polymer (dec n) (str gen b)))
                    (update gen dec))]
        #_(println "RES" res)
        (swap! results assoc-in [polymer n] res)
        res))))

(defn expand
  [n polymers]
  (loop [[head & [next :as rest]] polymers
         res {head 1}]
    (if (nil? next)
      res
      (->> (update (expand-polymer n (str head next)) head dec)
           (merge-with + res)
           (recur rest)))))


(defn part1 []
  (->> (expand 10 template)
       (reduce (fn [[vmax vmin] [_ count]] [(max vmax count) (min vmin count)]) 
               [0 (Integer/MAX_VALUE)])
       (apply -)))


(defn part2 [& [n]]
  (->> (expand (or n 40) template)
       (reduce (fn [[vmax vmin] [_ count]] [(max vmax count) (min vmin count)]) 
               [0 (Long/MAX_VALUE)])
       (apply -)))