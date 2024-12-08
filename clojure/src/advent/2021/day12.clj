(ns advent.2021.day12
  (:require [advent.core :as a]))

(def reader (a/read-input 2021 12))

(def input (->> (reader)
                (mapv (a/re-map #"([a-zA-Z]+)-([a-zA-Z]+)" [identity identity]))))
(def big-caves 
  (->>
   (flatten input)
   (filter #(re-matches #"[A-Z]+" %))
   (set)))

(def graph 
  (->> 
   input
   (reduce (fn [acc [from to]]
             (as-> acc $
               (if (= "start" to)
                 $
                 (update $ from conj to))
               (if (= "start" from)
                 $
                 (update $ to conj from))))
           {})))
(defn part1 
  ([] (part1 #{} "start"))
  ([visited node]
   (cond
     (= "end" node) 1
     :else (->>
            (graph node)
            (filter #(not (visited %)))
            (reduce (fn [acc next]
                      (->
                       (if (big-caves node)
                         visited
                         (conj visited node))
                       (part1 next)
                       (+ acc)))
                    0)))))

(defn part2
  ([] (part2 #{} nil "start" []))
  ([visited small-cave node path]
   (let [p (conj path node)]
     (->>
      (graph node)
      (reduce (fn [acc next]
                (->
                 (cond
                   (= "end" next) 1
                   (big-caves next) (part2 visited small-cave next p)
                   (visited next) (if (nil? small-cave) 
                                    (part2 visited next next p)
                                    0)
                   :else (-> (conj visited next) 
                             (part2 small-cave next p)))
                 (+ acc)))
              0)))))